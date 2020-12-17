; waiting queue, size 3
; infinte loop sending customers to queue, every 10-30ms)
; blocking barber queue, size 1, works 20ms
; how many haircuts in 10s

(def number-chairs 3)
(def opening-seconds (* 1000 10))
(def duration-haircut 20)
(def customer-spawn-time #(+ (rand-int 21) 10))

(def shop-open? (atom false))
(def open-chairs (ref number-chairs))
(def barber-busy? (ref false))
(def gone-customers (ref 0))
(def haircuts (ref 0))

(def customer-waiting? #(< @open-chairs number-chairs))
(def work-to-do? #(or @shop-open? (customer-waiting?)))

; current customer would be more imperative, just influence chairs

(defn generate-customers []
  (future
    (while @shop-open?
      (let [time-next-customer (customer-spawn-time)]
        ;(print time-next-customer)
        (Thread/sleep time-next-customer)
        (dosync
          (if (> @open-chairs 0)
            (alter open-chairs dec)
            (alter gone-customers inc)))))))

(defn give-haircut []
  (dosync
    (ref-set barber-busy? true)
    (alter open-chairs inc))
  (Thread/sleep duration-haircut)
  (dosync
    (ref-set barber-busy? false)
    (alter haircuts inc)))

(defn operate-shop []
  (future (while (work-to-do?) (if (not @barber-busy?) (give-haircut)))))

(defn open-shop []
  (reset! shop-open? true)
  (println "shop opened, customers are coming")
  (generate-customers)
  (operate-shop)
  (Thread/sleep opening-seconds)
  (reset! shop-open? false))

(println "starting barber test")
(open-shop)
(Thread/sleep (* 1.1 opening-seconds))
(println "ending barber test")
(println "haircuts:" @haircuts)
(println "gone customers:" @gone-customers)