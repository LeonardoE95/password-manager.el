# Password-Manager.el

This repository contains simple abstractions for interacting with
different password managers within the comfort of Emacs. For each
password manager, a simple `transient` based interface is also offered
to simplify the interaction flows.

As of right now, only the `Bitwarden` password manager is supported
and only in read mode. In the near future, I plan to add write mode
support for `Bitwarden` and also read and write support for `KeePass`.

For any feedback, either open an issue, or write an email.

# Quick Start

Load the main script `password-manager.el` into Emacs and you will
have access to the core functinalities. If you want the ui, simply
execute `pm-ui` which will trigger a simple transient UI.

The main functions for `bitwarden` are described below:

- `bw/login` uses the path of the bitwarden CLI binary taken from the
  variable `bw/binpath` to authenticate to the Bitwarden server.
  
- `bw/load-item`, used after authentication to load an item with all
  the data of interest (username, password, URIs).
  
- `bw/read-uris`, allows to copy into the system clipboard one of the
  URIs from the loaded item. Here the selection of the specific URI to
  copy is handled with `ivy`.
  
- `bw/read-username`, allows to copy into the system clipboard the
  username from the loaded item.
  
- `bw/read-password`, allows to copy into the system clipboard the
  password from the loaded item.
  
If your `bw` instance is found in a different path, change the variable `bw/binpath`

```elisp
(setq bw/binpath "<YOUR-PATH>")
```
  
# Security Considerations

Given the sensitive nature of the data in question, the following
considerations have driven the development.

- In order to avoid long-term exposure of sensitive data, after
  different amounts of time, the loaded data is cleaned. This is
  controlled using the variables `bw/session-timeout`,
  `bw/vault-timeout`, `bw/clipboard-timeout` and `bw/item-timeout`.
  
- It was decided that the `kill-ring` was not an appropriate data
  structure to leave sensitive data. Therefore, the system clipboard
  is used instead and it is cleaned one minute after sensitive data
  has been put into it. (I'm still not sure about this choice.)
  
- During first login and when unlocking the vault, the password is
  sent to the `bw` process always using the STDIN channel instead of
  the command line.

If you find that I'm missing something, of if you have insights that
can question these considerations, please let me know.

# TODOs

List of things that I need to fix:

- When calling `bw/load-item` I should check if the list of loaded
  items is still valid, and if its been cleaned I need to re-download
  it. This step could require unlocking the vault or doing a new login.

- Validate `cmd` input in `bw/cmd-anon-to-string` and `bw/cmd-auth-to-string`

- Catch error situations throughout the code.

