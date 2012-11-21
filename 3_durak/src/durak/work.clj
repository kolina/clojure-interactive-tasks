(ns durak.work
  (:use [durak.core :only (run-game)] ))


;;; Your task is to write bot for playing card game "Durak": http://en.wikipedia.org/wiki/Durak
;;; Bot plays against another built-in bot (so 2 players).
;;; Bot is map that contains 2 functions: function to attack and function to defend.
;;; It must have following format: {:attack YOUR_ATTACK_FN  :defend YOUR_DEFEND_FN}
;;; Attack and defend functions are similar: they both take same arguments and both must return card.
;;; Attack function is called when your are an attacker and it needs to select 1 card from your hand and put it on the table.
;;; Note that if it is not the first card in current attack, it must have the same rank as one of the cards on the table.
;;; If you return nil it means that you don't want to attack and attack is finished.
;;; Defend function is called  when your are a defender and it needs to select 1 card from your hand and put it on the table.
;;; If you return nil it means you can't (or don't want to) defend and you take all cards from the table.

;;; Card is a map with 2 keys: rank and suit.
;;; Rank is a number from 6 to 14.
;;; 11 - Jack
;;; 12 - Queen
;;; 13 - King
;;; 14 - Ace
;;; Suit is one of the 4 keywords: :spades :hearts :diamonds :clubs
;;; Examples: {:rank 6, :suit :clubs}  {:rank 14, :suit :hearts}

;;; Functions input:
;;; Each function takes single argument - map of following structure (example):
;;; {:hand  [{:rank 6, :suit :clubs}
;;;          {:rank 8, :suit :hearts}],
;;;  :table [{:rank 6, :suite :hearts}],
;;;  :tramp :clubs}
;;; Hand is a sequence of cards in your hand.
;;; Table is a sequence of cards already on table.
;;; If you are an attacker and it's start of an attack then table will be empty.
;;; If your are a defender then table will be non-empty and your need to beat last card from the table.
;;; Cards on the table can be represented like: attack  - defense - attack - defense - attack. Odd cards are attack, even cards are defense.
;;; Tramp is tramp suit of the game.

;;; To test your solution call (run-game YOUR_SOLUTION)
;;; Your bot is the player in the lower part of screen (cards a visible to your).
;;; To run next action press SPACE, to restart game press R.
;;; When game is over (one of the players has no cards in his hand) nothing will happen when your press SPACE.
;;; If your press SPACE and nothing happen and game is not over yet, look at stacktraces, probably your bot (or built-in bot) tries to perform invalid atttack or defense.


(defn can_defend[card tramp players_card]
  (cond (and (= (card :suit) (players_card :suit))
             (< (card :rank) (players_card :rank))) true
        (and (not=  (card :suit) tramp) (= (players_card :suit) tramp))
        	true
        :else false))

(defn less [tramp card_1 card_2]
  (cond (and (not= (card_1 :suit) tramp) (< (card_1 :rank) (card_2 :rank)))
        true
		(and (= (card_1 :suit) tramp) (= (card_2 :suit) tramp)
             (< (card_1 :rank) (card_2 :rank))) true
		(and (not= (card_1 :suit) tramp) (= (card_2 :suit) tramp)) true
        :else false))

(defn min_card [tramp player_cards]
  (reduce #(if (less tramp %1 %2) %1 %2) (first player_cards)
          (rest player_cards)))

(defn defend-fn[cur_state]
  (let [my_cards (cur_state :hand) cur_card (last (cur_state :table))
	deck-count (cur_state :deck-count)]
    (let [my_cards_defendable
         (filter #(can_defend cur_card (cur_state :tramp) %) my_cards)]
      (cond (empty? my_cards_defendable) nil
       :else 
	   (let [my_min_card (min_card (cur_state :tramp) my_cards_defendable)]
		(if (and (> deck-count 0) (and (= (my_min_card :tramp) 
			(cur_state :tramp)) (> (my_min_card :rank) 10)))
		nil my_min_card))))))

(defn can_atack[all_cards tramp players_card count-deck]
  (and (or (not= (players_card :suit) tramp) (= count-deck 0))  
       	(some #(= (players_card :rank) (% :rank)) all_cards)))

(defn atack-fn[cur_state]
  (let [my_cards (cur_state :hand) all_cards (cur_state :table) rest (cur_state :rest)]
    (let [my_cards_atackable
		  (filter #(can_atack all_cards (cur_state :tramp) % 
			(cur_state :deck-count)) my_cards)]
      (cond (empty? all_cards) (min_card (cur_state :tramp) my_cards)
       (empty? my_cards_atackable) nil
      :else (min_card (cur_state :tramp) my_cards_atackable)))))

(run-game {:attack atack-fn  :defend defend-fn})

;;; Implement program that takes 2 bots, runs game and return winner.
;;; Study init-game and next-action function in the src/durak/logic.clj. They can be used to run game.



;;; Implement bot that memorizes all cards played in game and use some smarter logic based on probability of opponents cards.
;;; Use clojure's atoms or refs to keep data between moves.
;;; If you've implemented this bot in the first task than you can skip it :)



;;; Implement attack and defencd functions that they ask user input. So human can play.
;;; I don't know how to do it, may be some hacks with swing like creating and invoking dialog to ask for user input.



;;; Modify program such that it can play with 3 or 4 players.
