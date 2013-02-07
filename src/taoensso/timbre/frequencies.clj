(ns taoensso.timbre.frequencies
  "Frequency logger for Timbre."
  {:author "Peter Taoussanis"}
  (:require [taoensso.timbre       :as timbre]
            [taoensso.timbre.utils :as utils]))

(def ^:dynamic *flog* "{::form-name {form-value frequency}}" nil)

(defmacro log-frequencies
  "When logging is enabled, executes named body with frequency counting enabled.
  Body forms wrapped in (fspy) will have their result frequencies logged. Always
  returns body's result.

  Note that logging appenders will receive both a frequencies table string AND
  the raw frequency stats under a special :frequency-stats key. One common use
  is for db appenders to check for this special key and to log frequency stats
  to db in a queryable manner."
  [level name & body]
  `(if (timbre/logging-enabled? ~level)
     `(binding [*flog* (atom {})]
        (let [result# (do ~@body)]
          (timbre/log* ~level {:frequency-stats @*flog*}
                       (str "Frequencies " (utils/fqname name)

                            )
                       )
          result#))
     (do ~@body)))

(defmacro fspy
  "Frequency spy. Evaluates named expression and returns the result. When in the
  context of a *flog* binding, records the frequency of each enumerated result.

  Defaults to :debug logging level and unevaluated expression as name."
  ([expr] `(fspy :debug ~expr))
  ([level expr] `(fspy ~level '~expr ~expr))
  ([level name expr]
     `(if (or (not (timbre/logging-enabled? ~level))
              (not *flog*))
        ~expr
        (let [name#   ~name
              result# ~expr]
          (swap! *flog* #(assoc-in % [name# result#]
                                   (inc (get-in % [name# result#] 0))))
          result#))))

(comment
  (with-frequencies
    (vec (repeatedly 20 (fn [] (fspy :info :rand-nth (rand-nth [:a :b :c])))))
    *flog*))