;;; pai-openrouter.el --- OpenRouter hosted provider for pai -*- lexical-binding: t; -*-

;;; Commentary:

;; Enables only OpenRouter.  Load automatically from `~/.pai/extensions/'
;; (home-wide) or a trusted project's `.pai/extensions/' directory.
;; Credentials use OPENROUTER_API_KEY, `pai-api-keys', or `/login openrouter'.
;; `/model' discovers available models; no static catalog is installed.

;;; Code:

(require 'pai)
(require 'pai-providers)

(pai-register-extension
 (lambda (_pi)
   (pai-register-provider-config
    '(:id "openrouter" :api openai-completions
      :base-url "https://openrouter.ai/api/v1" :env-key "OPENROUTER_API_KEY")))
 "openrouter")

(provide 'pai-openrouter)
;;; pai-openrouter.el ends here
