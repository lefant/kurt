# Accessing Kurt in an Amp orb

## Supported shell access

Amp does not currently provide an SSH hostname or connection string for an orb. The supported shell is the **Terminal** pane on the orb thread page. It opens a shared tmux session in the same filesystem and checkout used by the agent.

To test Kurt there:

```sh
printf 'name\nprotocol_version\nquit\n' | scripts/kurt-gtp
cabal v2-test
scripts/gtp-regression
```

You can also use the orb's **Desktop** pane for graphical Linux applications. Neither the Terminal nor Desktop pane requires an additional login or connection string.

Amp portals expose HTTP or HTTPS services. They do not expose an SSH daemon or Kurt's stdin/stdout GTP stream. Starting `sshd` and passing its port to `amp orb portal` will therefore not provide working SSH access.

## Optional temporary SSH shell with tmate

If browser Terminal access is unsuitable, `tmate` can create a temporary SSH connection through the public tmate service. This is an external service, not an Amp feature. Anyone who receives the read-write connection string can control the shell, so use it only for a short-lived debugging session and do not publish the string in repository files or public threads.

In the orb Terminal:

```sh
sudo apt-get update
sudo apt-get install -y tmate
amp orb service start tmate --command 'tmate -F'
amp orb service logs tmate
```

The logs print connection commands similar to:

```text
ssh <session-id>@<tmate-host>
```

Copy the read-write SSH command to your local terminal. Use the read-only command when the other participant only needs to observe the session.

Stop and invalidate the temporary session when finished:

```sh
amp orb service stop tmate
```

The connection opens tmate's shared terminal. It is not a general-purpose SSH endpoint and cannot be configured as a Sabaki or GoGui GTP engine command. For a desktop GTP client, continue to use `scripts/kurt-remote-gtp` with an ordinary SSH-reachable host, or build an authenticated HTTP/WebSocket-to-GTP bridge for an orb portal.

## Moving changes to a local checkout

If the goal is to inspect or test files locally rather than enter the orb, use Amp's supported sync command:

```sh
amp sync <thread-url-or-id>
```

This copies the orb thread's changes into the local checkout while the orb can continue running.
