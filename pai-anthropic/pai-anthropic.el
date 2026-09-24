;;; pai-anthropic.el --- Anthropic hosted provider for pai -*- lexical-binding: t; -*-

;;; Commentary:

;; Enables only Anthropic.  Load automatically from `~/.pai/extensions/'
;; (home-wide) or a trusted project's `.pai/extensions/' directory.
;; Credentials use ANTHROPIC_API_KEY or `pai-api-keys'.  `/login anthropic'
;; runs the Claude Pro/Max OAuth flow; OAuth tokens are sent as
;; `Authorization: Bearer' with the Claude Code identity, and refreshed
;; automatically.  `/model' discovers models (falling back to a built-in list
;; when the OAuth token can't list them).

;;; Code:

(require 'pai)
(require 'pai-providers)
(require 'pai-auth)

(defconst pai-anthropic--fallback-models
  '("claude-opus-4-5-20251101" "claude-sonnet-4-5-20250929"
    "claude-haiku-4-5-20251001" "claude-opus-4-1-20250805"
    "claude-sonnet-4-20250514")
  "Models offered when live discovery is unavailable (e.g. OAuth tokens that
cannot list models).")

(pai-register-extension
 (lambda (_pi)
   (pai-register-provider-config
    (list :id "anthropic" :api 'anthropic-messages
          :base-url "https://api.anthropic.com/v1" :env-key "ANTHROPIC_API_KEY"
          :models pai-anthropic--fallback-models))
   (setf (alist-get "anthropic" pai-auth-oauth-handlers nil nil #'equal)
         #'pai-auth-oauth-anthropic)
   (setf (alist-get "anthropic" pai-auth-oauth-refresh-handlers nil nil #'equal)
         #'pai-auth-oauth-anthropic-refresh))
 "anthropic")

(provide 'pai-anthropic)
;;; pai-anthropic.el ends here
