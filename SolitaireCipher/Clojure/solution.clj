
(defn remove-non-alphabetic
  "Given a string, returns a new string that is the same as the input string except that all 
  non-alphabetic characters have been removed.
  ex) (remove-non-alphabetic \"hello world!1\") -> \"HELLOWORLD\"

  @param input: String -> The string to remove non-alphabetic characters from and capitalize
  "
  [input]
  (.toUpperCase (apply str (filter 
   #(Character/isLetter %) input))))

(defn pad-string
  "If the passed string's length is not a multiple of 5, returns a string padded with X's 
  such that it is a multiple of 5
  ex) (pad-string ASDF) -> ASDFX

  @param x: String -> The string to pad
  @return The padded string "
  [x]
  (let [padding (mod (count x) 5)]
    (apply str x (take (- 5 padding) (repeat "X")))))
    

(defn convert-to-num
  "Given a string, returns a list where each character has been replaced by an int normalized by
  its place relative to the letter A. Only works for alphabetic capital letters.
  ex) ABCDE -> (1 2 3 4 5)

  @param input: String -> A string only containing capital letters
  @return A list where each character in the string is translated to an int representing its distance
          to the letter A
  "
  [input]
  (let [base (- (int \A) 1)]
    (map #(- (int %) base) input)))

(defn deck
  "Returns a vector containing an unshuffled deck of cards
  A deck is a vector containing all numbers from 1 to 52 plus the char A and B (representing the 
  two jokers)"
  []
  (conj (vec (range 1 53)) \A \B))

(defn swap 
  "Swaps two elements in a vector.
  source: http://stackoverflow.com/questions/5979538/what-is-the-idiomatic-way-to-swap-two-elements-in-a-vector"
  [v i1 i2]
  (assoc v i2 (v i1) i1 (v i2)))

(defn move-joker
  "Moves a joker up one place in the deck. If the joker is the last place in the deck, its moved
  to the *second* place from the front.
  ex) (move-joker [1 2 3 4 5 A B] A) -> [1 2 3 4 5 B A]
  ex2) (move-joker [1 2 3 4 5 A B] B) -> [1 B 2 3 4 5 A]

  @param deck: vector -> A vector containing the deck
  @param joker: Char -> A char (either A or B) representing which joker to move
  "
  [deck joker]
  (let [joker-index (.indexOf deck joker)
        deck-length (count deck)
        incr-joker-index (+ joker-index 1)]
    (if (>= incr-joker-index deck-length)
      (into [] (concat [(first deck)] [joker] (drop 1 (drop-last deck))))   ;need to move joker to front of deck
      (swap deck joker-index incr-joker-index))))   ;just move joker up one

(defn move-joker-n-places
  "Moves the passed joker backward in the passed deck n places. Note: if the joker is moved past the 
  last card in the deck, it will wrap to the *second* card from the front.
  ex) (move-joker 1 B [1 2 3 4 B]) -> [1 B 2 3 4] (notice the B goes below the 1)
  ex2) (move-joker 2 B [1 2 3 B 4 5 A]) -> [1 2 3 4 5 B A]

  @param n: int -> The number of places to move backwards
  @param joker: char -> The joker to move (A or B)
  @param deck: vector -> The original deck
  @return Vector with joker moved forward n places
  "
  [n joker deck]
  (loop [iter n
         new-deck deck]
    (if (== 0 iter)
      new-deck
      (recur (- iter 1) (move-joker new-deck joker)))))

(defn triple-cut
  "Performs a triple cut on the passed deck. A triple cut is where the sequence of cards to the 
  *left* of the first joker in the deck and the sequence of cards to the *right* of the second 
  joker in the deck are swapped.
  ex) (triple-cut [1 2 3 B 4 5 6 A 7 8 9]) -> [7 8 9 B 4 5 6 A 1 2 3]
  
  @param deck: vector -> The deck to perform the triple cut upon
  @return vector The new deck"
  [deck]
  (let [a-joker-index (.indexOf deck \A) ;Get the indices of our two jokers
        b-joker-index (.indexOf deck \B) 
        order-func #(if (%1 %2 %3) %2 %3) ;Figure out which one is bigger
        first-index (order-func < a-joker-index b-joker-index)
        second-index (order-func > a-joker-index b-joker-index)
        first-cut (take first-index deck)        ;partition the deck around the jokers
        second-cut (drop (+ second-index 1) deck)
        middle-cut (take (- second-index (- first-index 1)) (drop first-index deck))]
    (into [] (concat second-cut middle-cut first-cut))))
    
(defn count-cut 
  "Performs a count cut on the passed deck and returns the new deck. A count cut is where
  we use the value of the bottom card to determine how many cards to take from the top of 
  the deck. The cards then taken from the top are placed just *above* the bottom card in the deck.
  ex) (count-cut [B 2 3 4 6 A 1]) -> [2 3 4 5 6 A B 1] (take 1 from the top and place it below the 1)
      (count-cut [1 2 4 5 A B 3]) -> [5 A B 1 2 4 3] (take 3 from the top and place it below the 3)

  @param deck: vector -> The deck to perform the count cut on
  @return vector -> The new deck"
  [deck]
  (let [[rest-of-deck bottom-card] (split-at (- (count deck) 1) deck)
        num-cards (if (number? (first bottom-card)) (first bottom-card) 53)
        [cut rest] (split-at num-cards rest-of-deck)]
    (into [] (concat (vec rest) (vec cut) (vec bottom-card)))))

(defn convert-to-letter
  "Converts the passed int parameter to a char, normalized such that the char A is 1.
  ex) (convert-to-letter 1) -> A
      (convert-to-letter 26) -> Z

  @param n: int -> The int to turn into a letter
  @return char The letter corresponding to the passed into"
  [n]
  (char (+ n 64)))

(defn get-output-letter
  "Returns the next letter in the keystream.
  Converts the top card to it's value and gets the card indexed to that value from the top
  of deck. If that card is a number, its converted to a char. If its a joker, its nil.

  ex) (get-output-letter [2 3 4 52 A B 1]) -> D (the top card is 2, so get the card indexed at 2 in
                                                 the deck. This card is 4, which corresponds to the
                                                 char D, since D is the fourth letter in the 
                                                 alphabet)  
  
  @param deck: vector -> The deck to find the next letter
  @return char or nil -> Char if a card was found; nil if a joker was found"
  [deck]
  (let [top-card (first deck)
        top-card-num (if (number? top-card) top-card 53)
        nth-letter (deck top-card-num)]
    (cond
      (char? nth-letter) nil
      (< 26 nth-letter) (convert-to-letter (- nth-letter 26))
      :else (convert-to-letter nth-letter))))

(defn generate-keystream
  "Returns a keystream.

  @param deck: vector -> The deck to use to generate the keystream
  @param length: int -> The length of the returned keystream
  @return String -> The keystream
  "
  [deck length]
  (loop [result []
         cur-deck deck]
    (if (== (count result) length)
      (clojure.string/join "" result)
      (let [new-deck (count-cut (triple-cut (move-joker-n-places 2 \B (move-joker-n-places 1 \A cur-deck))))
            new-letter (get-output-letter new-deck)]
        (if (nil? new-letter)
          (recur result new-deck)
          (recur (conj result new-letter) new-deck))))))


(defn zip2
  "Returns a list of two element vectors that contain corresponding elements from the two passed
  collections. It will drop any extra elements.
  (zip2 [1 2 3] [4 5 6]) -> ([1 4] [2 5] [3 6])
  (zip2 [1 2 3] [4 5 6 7]) -> ([1 4] [2 5] [3 6])

  @param col1: seq -> The first sequence
  @param col2: seq -> The second sequence
  @return A list of two element vectors where each vector contains corresponding elements from the
          two passed sequences.
  "
  [col1 col2]
  (map vector col1 col2))
  

(defn transform-message
  "Transforms the passed message using the passed keystream. 
  
  @param message: String -> The message to transform
  @param keystream: String -> The keystream to use
  @param f: (coll => int) -> A function that takes a collection and returns a number
  "
  [message keystream f]
  (let [message-nums (convert-to-num (remove-non-alphabetic message))
        keystream-nums (convert-to-num keystream)
        zipped-nums (zip2 message-nums keystream-nums)
        added-nums (map f zipped-nums)]
    (apply str (map #(convert-to-letter %) added-nums))))

(defn decrypt
  "Decrypts the passed message using the passed keystream. If the passed keystream does not
  correspond to the keystream that was used to encrypt the message, the returned string will
  *not* be the correct message.

  @param message: String -> The message to decrypt
  @param keystream: String -> The keystream that can decrypt this message
  @return String -> The decrypted message"
  [message keystream]
  (defn add-zipped
    [coll]
    (let [ans (reduce - coll)]
      (if (> 0 ans)
        (+ ans 26)
        ans)))
  (transform-message message keystream add-zipped))

(defn encrypt
  "Encrypts the passed message using the passed keystream.
  
  @param message: String ->   The message to encrypt
  @param keystream: String -> The keystream to use to encrypt the message
  @return String -> The encrypted message
  "
  [message keystream]
  (defn sub-zipped
    [coll]
    (let [ans (reduce + coll)]
      (if (< 26 ans)
        (- ans 26)
        ans)))
  (transform-message message keystream sub-zipped))
  
;basic decryption and encryption functions using keystream from unshuffled deck
(defn basic-encrypt
  [message]
  (let [unkeyed-deck (deck)]
    (encrypt message (generate-keystream unkeyed-deck (count unkeyed-deck)))))

(defn basic-decrypt
  [message]
  (let [unkeyed-deck (deck)]
    (decrypt message (generate-keystream unkeyed-deck (count unkeyed-deck)))))

;;Main method.
(defn main
  []
  (do
    (println "Enter 'e' for encrypt or 'd' for decrypt")
  (let [action (read-line)
        function 
        (cond 
          (= action "e") basic-encrypt
          (= action "d") basic-decrypt
          :else (do 
                  (println "unknown action. Exiting.")
                  (System/exit 1)))]
    (println "Enter message:")
    (let [message (read-line)]
      (println (function message))))))
    
(main)
