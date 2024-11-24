# Password-Manager.el

This repository contains simple abstractions that can be used to
interact with different password managers within the comfort of
Emacs. For each password manager, a simple `transient` based interface
will be offered to ease out the interaction flows.

As of right now, only the `Bitwarden` password manager is
supported. In the future, I will also want to support `KeePass` and
maybe other password managers.

If you have any feedback, feel free to open issues or let me know by
email.

# Quick Start

Execute the script, and then will have access to the core
functinalities. If you want the ui, simply execute `bw-ui` which will
trigger a simple transient UI.

Otherwise, the main functions are described below:

- `bw/login` to perform login. It uses the hard-coded path of the bitwarden CLI binary
  `"/usr/bin/bw"` so be careful if its different in your system.
  
- `bw/load-item`, can be used after authentication to load an item
  with all the data of interest.
  
- `bw/read-uris`, allows to copy into the system clipboard one of the
  URIs from the loaded item. Here the selection of the specific URI to
  copy is handled with `ivy`.
  
- `bw/read-username`, allows to copy into the system clipboard the
  username from the loaded item.
  
- `bw/read-password`, allows to copy into the system clipboard the
  password from the loaded item.
  
# Security Considerations

Given the sensitive nature of the data in question, the following
considerations have driven the development.

- In order to avoid long-term exposure of sensitive data, after
  different amounts of time, the loaded data is cleaned. Specifically,
  this is controlled using the variables `bw/session-timeout`,
  `bw/vault-timeout`, `bw/clipboard-timeout` and `bw/item-timeout`.
  
- It was decided that the `kill-ring` was not an appropriate data
  structure to leave sensitive data. Therefore, the system clipboard
  is used instead and it is cleaned one minute after sensitive data
  has been put into it. (I'm still not sure about this choice.)
  
- The password is sent to the `bw` process using the STDIN channel
  instead of using the command line.

If you find that I'm missing something, of if you have insights that
can question these considerations, please let me know by opening an
issue.

# TODOs

List of things that I need to fix:

- When calling `bw/load-item` I should check if the list of loaded
  items is still valid, and if its been cleaned I need to re-download
  it. This step could require unlocking the vault or doing a new login.

- Catch error situations throughout the code.
