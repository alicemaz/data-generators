(use sentry sentry-notify-emacs sentry-plugin-test)

(sentry
 (sentry-test
  '((".*.scm" . "tests/run.scm"))))
