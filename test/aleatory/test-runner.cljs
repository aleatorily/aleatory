(ns aleatory.test-runner

  (:require [doo.runner :refer-macros [doo-tests]]
            [aleatory.prng.source-test])

  )


(doo-tests 'aleatory.prng.source-test)

