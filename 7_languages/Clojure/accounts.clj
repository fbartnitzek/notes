; use refs to create a vector of accounts in memory
; create debit and credit functions to change the balance of an account
; debit -
; credit +

(def accounts (ref [1 2 3 4]))

(defn credit
  [accounts idx amount]
  (dosync
    (alter accounts update idx + amount)))

(defn debit
  [accounts idx amount]
  (dosync
    (alter accounts update idx - amount)))