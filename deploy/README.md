# Hosting Drone Biomass Studio at dronebior.com

One container per logged-in user, on a Google Compute Engine VM, reached through
a Cloudflare Tunnel. No port is opened on the VM.

```
browser ── dronebior.com ── Cloudflare Access ── Tunnel ── cloudflared
                                                              │  (docker network)
                                                          ShinyProxy :8080
                                                              │  starts one per user
                                                     ┌────────┴────────┐
                                                  studio:hugo      studio:ana
                                                   /data  /spool    /data  /spool
                                                              │  a file, not a call
                                                            broker  ── docker.sock ── ODM
```

## Why the broker exists

`R/engine_odm.R` builds the reconstruction command as

```r
"-v", paste0(dataset_dir, ":/datasets")
```

and `dataset_dir` comes from the project the user chose. On a laptop that is
fine — the person driving the application already owns the machine. On a shared
server it is not: a container that can reach the Docker socket, plus a
user-controlled path in a `-v` argument, means any user can mount any host path
into a container that runs as root. **Giving a user-facing container the Docker
socket is handing out root on the host.**

So the user's container does not get one. It writes a job request into its own
spool directory; the broker — which has the socket, no port, and no network at
all — reads it, and:

1. **takes the user from the spool directory the file arrived in**, never from
   the request. Nothing a user writes changes who the broker thinks they are.
2. **binds a directory that contains no user input at all.** This is stronger
   than "the path is checked", and the difference is not academic — an
   adversarial review broke the checked version twice. Validating
   `project_dir` and then mounting `project_dir + odm_dataset_subdir` meant a
   symlink named in that second field produced `-v /:/datasets`. And even the
   right path can be swapped for a symlink between the check and the mount,
   because the user owns their whole subtree.

   So the mount is always `users/<user>` — computed from the spool directory
   the request arrived in, and nothing else — and the project is addressed
   *inside* the container with `--project-path`. A symlink planted in the mount
   then resolves against the container's filesystem, where it reaches nothing.
   `build_odm_args()` refuses to run at all if a code path forgets to pass the
   pin, so a new caller fails loudly instead of quietly mounting a user path.
3. **chooses parameters from a fixed table** rather than forwarding them. An
   unknown name is dropped; a known one is coerced and clamped. Nothing a user
   writes becomes a token on a `docker` command line.

## 1. The VM

Sizing is from measurements on a real flight, not a guess. On the ifasbahia10
survey (1,533 images) OpenDroneMap with 9 auto-selected workers reached ~45 GB
and was OOM-killed; 3 workers at the lowest detail level peaked at 5.4 GB and
finished. Memory scales with the worker count, which is why the broker caps it
(`DRONEBIOR_MAX_WORKERS`).

| | vCPU | RAM | good for |
|---|---|---|---|
| `n2-highmem-4` | 4 | 32 GB | one reconstruction at a time, small team |
| `n2-standard-8` | 8 | 32 GB | faster reconstruction, same memory headroom |
| `n2-highmem-8` | 8 | 64 GB | two concurrent reconstructions, comfortable |

Disk: a flight is roughly 12 GB of imagery plus ~2 GB of products, so 500 GB
balanced is a sensible start and can be grown in place.

```bash
gcloud compute instances create dronebior \
  --project=YOUR_PROJECT --zone=us-east1-b \
  --machine-type=n2-standard-8 \
  --image-family=ubuntu-2404-lts-amd64 --image-project=ubuntu-os-cloud \
  --boot-disk-size=500GB --boot-disk-type=pd-balanced \
  --metadata=enable-oslogin=TRUE
```

Note what is *not* here: no `--tags`, no firewall rule, no external IP needed
for serving. The tunnel dials out. You can even add `--no-address` if you set up
Cloud NAT for the outbound side.

**Cost is per hour of uptime.** A 32 GB VM running continuously is the largest
line item in this whole setup; check the current figure in Google's pricing
calculator rather than trusting a number written here. If the site only needs to
be up during working hours, `gcloud compute instances stop/start` is enough —
the tunnel reconnects on its own, and DNS never changes.

## 2. Docker and the directories

```bash
gcloud compute ssh dronebior --zone=us-east1-b

sudo apt-get update && sudo apt-get install -y docker.io docker-compose-v2 git
sudo systemctl enable --now docker
sudo usermod -aG docker "$USER" && exec newgrp docker

# One data directory and one spool per user, each owned by a DIFFERENT uid.
# The directory name must match the ShinyProxy user name exactly - that is the
# link the broker relies on to know who a job belongs to.
sudo mkdir -p /var/log/dronebior
for pair in hugo:10001 ana:10002 carlos:10003; do
  u=${pair%%:*}; uid=${pair##*:}
  sudo mkdir -p /srv/dronebior/users/$u /srv/dronebior/spool/$u
  sudo chown -R "$uid:$uid" /srv/dronebior/users/$u /srv/dronebior/spool/$u
  sudo chmod 700 /srv/dronebior/users/$u /srv/dronebior/spool/$u
done
# The parents stay root-owned. That is what makes users/<user> unswappable:
# replacing it would need write permission here, and no user has it.
sudo chown root:root /srv/dronebior /srv/dronebior/users /srv/dronebior/spool
sudo chmod 755 /srv/dronebior /srv/dronebior/users /srv/dronebior/spool
```

Two things here are load-bearing and easy to undo by accident.

**A different uid per user.** An earlier version of this file gave every user
uid 10001, which made the ownership barrier decorative: the volumes were then
the only thing separating three people, and a volume list is one typo away from
mounting the wrong tree. With distinct uids, that typo is refused by the kernel
rather than by a config file. The uids must match `docker-user` in
`application.yml` and `DRONEBIOR_UID_MAP` in `docker-compose.yml`;
`deploy/test/isolation-test.R` fails if the three ever disagree.

**Root-owned parents.** `users/<user>` is the directory bound into the ODM
container, and the whole mount argument rests on the user being unable to
replace it with a symlink. They cannot, because that needs write permission on
`users/`, which is root's. Loosen that and the pinned mount stops being pinned.

## 3. Build and configure

```bash
git clone https://github.com/HugoMachadoRodrigues/DroneBioR.git
cd DroneBioR/deploy

docker build -t dronebior/studio:0.6.0 --build-arg DRONEBIOR_REF=v0.6.0 ./app
docker compose build broker
docker pull opendronemap/odm:latest      # the broker runs this; pre-pull it
```

Set each password before the first start — `application.yml` ships with
`CHANGE_ME_<user>` placeholders on purpose, and ShinyProxy will happily run
with them:

```bash
for u in hugo ana carlos; do
  sed -i "s/CHANGE_ME_$u/$(openssl rand -base64 24)/" shinyproxy/application.yml
done
grep -A2 'name: ' shinyproxy/application.yml    # write them down, then share privately
! grep -q CHANGE_ME shinyproxy/application.yml && echo "no placeholders left"
```

Then check the three places that must agree on uids before anything starts:

```bash
Rscript ../deploy/test/isolation-test.R    # 26 cases, no Docker needed
```

## 4. The tunnel

In the Cloudflare dashboard: **Zero Trust → Networks → Tunnels → Create**, name
it `dronebior`, and copy the token. Then add a **public hostname**:

| field | value |
|---|---|
| Subdomain | *(blank, or `app`)* |
| Domain | `dronebior.com` |
| Service | `HTTP` |
| URL | `shinyproxy:8080` |

`shinyproxy` resolves because both containers sit on `dronebior_net`. Creating
the hostname writes the DNS record for you — do not add a CNAME by hand.

```bash
echo "TUNNEL_TOKEN=eyJhIjoi...paste..." > .env
chmod 600 .env
docker compose up -d
docker compose logs -f cloudflared    # expect "Registered tunnel connection"
```

Two settings on the hostname's **Additional application settings** are worth
changing, because Shiny talks over a WebSocket that stays open for the length of
a reconstruction:

- **Connect timeout** 60s, **keep-alive timeout** 120s.
- Leave `--no-autoupdate` in the compose command (it is already there): a
  cloudflared self-update restarts the process and drops every open WebSocket,
  which to a user looks like the app dying three hours into a run.

## 5. Access

The tunnel makes the site reachable; it does not decide who may reach it. Add a
policy in **Zero Trust → Access → Applications → Add → Self-hosted**:

| field | value |
|---|---|
| Application domain | `dronebior.com` |
| Policy action | Allow |
| Include | Emails — the addresses that should get in |
| Session duration | 24h |

With email OTP as the identity provider nobody needs an account anywhere; a
one-time code goes to the address. If UF publishes SAML or OIDC, add it as an
identity provider and switch the rule to a group.

Access is the outer layer and ShinyProxy's own login is the inner one. Two
layers is deliberate: a mistake in an Access policy should not by itself hand
over the data.

## 6. Adding a user

Pick an unused uid, then make all three places agree:

```bash
u=maria; uid=10004
sudo mkdir -p /srv/dronebior/users/$u /srv/dronebior/spool/$u
sudo chown -R "$uid:$uid" /srv/dronebior/users/$u /srv/dronebior/spool/$u
sudo chmod 700 /srv/dronebior/users/$u /srv/dronebior/spool/$u
```

1. `application.yml`: add to `users:` with `groups: [u_maria]`, and copy a
   `specs:` entry with `docker-user: "10004"` and that user's two volumes.
2. `docker-compose.yml`: append `maria:10004` to `DRONEBIOR_UID_MAP`.
3. Add their email to the Cloudflare Access policy.
4. `Rscript deploy/test/isolation-test.R` — it checks the three agree before
   you restart anything.
5. `docker compose up -d`

A user with no uid in the map is refused a reconstruction rather than run as
root, so a half-finished addition fails safely.

## 7. Getting imagery onto the VM

The application takes a *project* from `/data`, not an upload — 12 GB of images
is not something to push through a browser. Copy them in first:

```bash
gcloud compute scp --recurse ./voo-2026-05-01 \
  dronebior:/tmp/voo-2026-05-01 --zone=us-east1-b
gcloud compute ssh dronebior --zone=us-east1-b --command \
  'sudo mv /tmp/voo-2026-05-01 /srv/dronebior/users/hugo/ && \
   sudo chown -R 10001:10001 /srv/dronebior/users/hugo/voo-2026-05-01'
```

A project directory holds the imagery where the application expects it —
`imagens/micasense/` for a MicaSense flight, or the DJI folder as the camera
wrote it.

## What to check when something is wrong

| symptom | look at |
|---|---|
| Error 1016 at the domain | `docker compose logs cloudflared` — the tunnel is not connected |
| Login works, app never starts | `docker compose logs shinyproxy`, and that the image tag matches `container-image` |
| App starts, reconstruction never begins | `docker compose logs broker`, and that `/srv/dronebior/spool/<user>` exists and is owned by 10001 |
| "that path resolves outside your own directory" | the broker refusing a project outside the user's own tree — working as intended |
| Session dies mid-reconstruction | `heartbeat-timeout` in `application.yml`, and whether cloudflared restarted |

## Known limits

* **One reconstruction at a time per user**, and the broker runs them serially
  across all users. Two people starting a run means the second waits. Making
  that concurrent needs a queue with a memory budget, not just more threads —
  two 45 GB runs on a 32 GB VM is an OOM kill, not slow progress.
* **The project field is a list, not free text**, in the hosted build. That is
  the point: on a shared server a text box that accepts any path is a way to
  read the file system.
* **ShinyProxy holds the Docker socket.** That is inherent to it starting
  containers, and it is acceptable because users do not run code inside
  ShinyProxy — only inside the containers it starts. The broker holds it for the
  same reason and is likewise unreachable.
