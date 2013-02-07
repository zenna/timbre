(ns taoensso.timbre.profiling
  "Logging profiler for Timbre, adapted from clojure.contrib.profile."
  {:author "Peter Taoussanis"}
  (:require [taoensso.timbre       :as timbre]
            [taoensso.timbre.utils :as utils]))

(def ^:dynamic *plog* "{::pname [time1 time2 ...] ...}" nil)

(declare p plog-stats plog-table)

(defmacro profile
  "When logging is enabled, executes named body with profiling enabled. Body
  forms wrapped in (p) will be timed and time stats logged. Always returns
  body's result.

  Note that logging appenders will receive both a profiling table string AND the
  raw profiling stats under a special :profiling-stats key. One common use is
  for db appenders to check for this special key and to log profiling stats to
  db in a queryable manner."
  [level name & body]
  `(if-not (timbre/logging-enabled? ~level)
     (do ~@body)
     (let [name# (utils/prepare-name ~name)]
       (binding [*plog* (atom {})]
         (let [result# (p ::clock-time ~@body)
               stats#  (plog-stats @*plog*)]
           (timbre/log* ~level
                        {:profile-stats stats#}
                        (str "Profiling " (utils/fqname name#))
                        (str "\n" (plog-table stats#))))))))

(defmacro sampling-profile
  "Like `profile`, but only enables profiling every 1/`proportion` calls.
  Always returns body's result."
  [level proportion name & body]
  `(if-not (> ~proportion (rand))
     (do ~@body)
     (profile ~level ~name ~@body)))

(defmacro p
  "When in the context of a *plog* binding, records execution time of named
  body. Always returns the body's result."
  [name & body]
  (let [name (utils/prepare-name name)]
    `(if-not *plog*
       (do ~@body)
       (let [start-time# (System/nanoTime)
             result#     (do ~@body)
             elapsed#    (- (System/nanoTime) start-time#)]
         (swap! *plog* #(assoc % ~name (conj (% ~name []) elapsed#)))
         result#))))

(defn plog-stats
  "{::pname [time1 time2 ...] ...} => {::pname {:min <min-time> ...} ...}"
  [plog]
  (reduce (fn [m [pname times]] ; TODO reduce-kv for Clojure 1.4+
            (let [count (count times)
                  time  (reduce + times)
                  mean  (long (/ time count))
                  ;; Mean Absolute Deviation
                  mad   (long (/ (reduce + (map #(Math/abs (long (- % mean)))
                                                times))
                                 count))]
              (assoc m pname {:count count
                              :min   (apply min times)
                              :max   (apply max times)
                              :mean  mean
                              :mad   mad
                              :time  time})))
          {} plog))

(defn plog-table
  "Returns formatted table string for given plog stats."
  ([stats] (plog-table stats :time))
  ([stats sort-field]
     (let [;; How long entire (profile) body took
           clock-time (-> stats ::clock-time :time)
           stats      (dissoc stats ::clock-time)

           accounted (reduce + (map :time (vals stats)))
           max-name-width (apply max (map (comp count str)
                                          (conj (keys stats) "Accounted Time")))
           pattern   (str "%" max-name-width "s %6d %9s %10s %9s %9s %7d %1s%n")
           s-pattern (.replace pattern \d \s)

           perc #(Math/round (/ %1 %2 0.01))
           ft (fn [nanosecs]
                (let [pow     #(Math/pow 10 %)
                      ok-pow? #(>= nanosecs (pow %))
                      to-pow  #(utils/round-to %2 (/ nanosecs (pow %1)))]
                  (cond (ok-pow? 9) (str (to-pow 9 1) "s")
                        (ok-pow? 6) (str (to-pow 6 0) "ms")
                        (ok-pow? 3) (str (to-pow 3 0) "μs")
                        :else       (str nanosecs     "ns"))))]

       (with-out-str
         (printf s-pattern "Name" "Calls" "Min" "Max" "MAD" "Mean" "Time%" "Time")

         (doseq [pname (->> (keys stats)
                            (sort-by #(- (get-in stats [% sort-field]))))]
           (let [{:keys [count min max mean mad time]} (stats pname)]
             (printf pattern (utils/fqname pname) count (ft min) (ft max) (ft mad)
                     (ft mean) (perc time clock-time) (ft time))))

         (printf s-pattern "[Clock] Time" "" "" "" "" "" 100 (ft clock-time))
         (printf s-pattern "Accounted Time" "" "" "" "" ""
                 (perc accounted clock-time) (ft accounted))))))

(comment
  (profile :info :Sleepy-threads
           (dotimes [n 5]
             (Thread/sleep 100) ; Unaccounted
             (p :1ms  (Thread/sleep 1))
             (p :2s   (Thread/sleep 2000))
             (p :50ms (Thread/sleep 50))
             (p :rand (Thread/sleep (if (> 0.5 (rand)) 10 500)))
             (p :10ms (Thread/sleep 10))
             "Result"))

  (p :hello "Hello, this is a result") ; Falls through (no *plog* context)

  (defn my-fn
    []
    (let [nums (vec (range 1000))]
      (+ (p :fast-sleep (Thread/sleep 1) 10)
         (p :slow-sleep (Thread/sleep 2) 32)
         (p :add  (reduce + nums))
         (p :sub  (reduce - nums))
         (p :mult (reduce * nums))
         (p :div  (reduce / nums)))))

  (profile :info :Arithmetic (dotimes [n 100] (my-fn)))

  (sampling-profile :info 0.2 :Sampling-test (p :string "Hello!")))