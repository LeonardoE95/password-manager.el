# Password-Manager.el

This repository contains simple abstractions for interacting with
different password managers within the comfort of Emacs.

Currently, the following password managers are supported:

- `Bitwarden`

In the future I plan to support for other managers.

**Disclaimer**: This is a work in progress, so expect bugs and weird
behaviors. For any feedback, either open an issue, or write an email.

# Quick Start

The following elisp will suffice to load the package.

```elisp
(add-to-list 'load-path "/home/leo/projects/GIT-PUBLIC/password-manager.el/src/")
(require 'password-manager)

(when env/work
  (setq bitwarden/binpath "/snap/bin/bw")
  (setq bitwarden/email "you@email.com")
  )
```

You can then access the main `transient` interface with the command `pm-ui`. 
  
# Security Considerations

Given the sensitive nature of the data in question, the following
considerations have driven the development.

1. In order to avoid long-term exposure of sensitive data, the loaded
   data is claned after a certain amount of time. This amount can be
   controlled using different variables, depending on the data in
   question, such as `bitwarden/session-timeout`,
   `bitwarden/vault-timeout`, `bitwarden/clipboard-timeout` and
   `bitwarden/item-timeout`.
   
2. It was decided that the `kill-ring` was not an appropriate data
   structure to leave sensitive data. Therefore, the system clipboard
   is used instead and it is cleaned one minute after sensitive data
   has been put into it. (I'm still not sure about this choice.).
   
3. During first login and when unlocking the vault, the password is
   sent to the `bw` process always using the STDIN channel instead of
   the command line.

If you find that I'm missing something, of if you have insights that
you want to share, please let me know.
