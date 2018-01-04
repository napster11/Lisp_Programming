;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Shivam Singh (5414019)
;; Course: CSCI-5511
;; Instructore: Nikos Papanikolopolous
;; Problem: Missionary Cannibal Problem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check if current state is a goal state or not
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun goalstate(node nl)
    (if (= nl 24) (setq goal '((0 0) (24 24))) (setq goal '((0 0) (15 15))))  ;; To define the goal state for 15 and 24 cannibal problem
    ;; (setq goal '((0 0) (24 24)))
    (setq temp (subseq node 0 2))
    (equal goal temp)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculate the heuristic value of current node. H = (no. of peoples on the initial state)/(Boat Capacity)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun heuristic(a b)
    (setq result (/ (+ a b) 6))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sort the list in increasing order based on the (g+h) value
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sortMin(ls)
    (sort ls #'(lambda (a b) (< (+ (first (third a)) (second (third a))) (+ (first (third b)) (second (third b))))))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check if using the given operator, can we generate possible successors or not
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun feasibleOperation(node op dir)

    (setq fsM (first (first node)))
    (setq fsC (second (first node)))
    (setq scM (first (second node)))
    (setq scC (second (second node)))
    (setq opM (first op))
    (setq opC (second op))

    (setq missionaryA (- fsM (* opM dir)))
    (setq cannibalA (- fsC (* opC dir)))
    (setq missionaryB (+ scM (* opM dir)))
    (setq cannibalB (+ scC (* opC dir)))

    ;; No. of Missionaries should be greater than or equal to Cannibals on both sides plus on the boat too
    (if (or (< missionaryA 0) (< cannibalA 0) (< missionaryB 0) (< cannibalB 0) (and (> missionaryA 0) (< missionaryA cannibalA)) (and (> missionaryB 0) (< missionaryB  cannibalB))) 0 1)

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find the successor of the current state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun findchilds (root dir)

    (setq result nil)
    (setq ans nil)
    (setq fsM (first (first root)))
    (setq fsC (second (first root)))
    (setq scM (first (second root)))
    (setq scC (second (second root)))
    (setq g (first (third root)))
    (setq h (second (third root)))

    ;; Apply the given operators on any state and generate the new possible ones
    (setq op '((1 0) (0 1) (1 1) (0 2) (2 0)  (2 1) (2 2) (0 3) (3 0) (3 1) (3 2) (3 3) (0 4) (4 0) (4 1) (4 2) (0 5) (5 0) (5 1) (0 6) (6 0)))

    (dolist (operator op)

        (cond
            ;;Check if it's possible to generate a new successor by applying the operator on current node
            ((= (feasibleOperation root operator dir) 1)
                (setq opM (first operator))
                (setq opC (second operator))
                (setq missionaryA (- fsM (* opM dir)))
                (setq cannibalA (- fsC (* opC dir)))
                (setq missionaryB (+ scM (* opM dir)))
                (setq cannibalB (+ scC (* opC dir)))
                (setq temp (list (list missionaryA cannibalA) (list missionaryB cannibalB) (list (+ 1 g) (heuristic missionaryA cannibalA)) ))
                (setq ans (append ans (list temp)))
            ) 
        )
    )
    (setq result ans)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A* algorithm to reach the Goal State using given intital state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MissionaryCannibal(l nt)

    (setq l (append l (list '(0 0))))
    (setq nl nt)
    (setq vplist nil)                   ;;Visited List for Positive Side
    (setq vnlist nil)                   ;;Visited List for Negative Side
    (setq result nil)
    (setq nlist nil)                    ;;NodeList for Negative Side
    (setq plist (append (list l)))      ;;NodeList for Positive Side
    
    (setq count 0)                      ;; Store the no. of steps taken to reach to the goal state
    (setq expandedNodes 0)              ;; No. of nodes expanded to reach to the goal state
    (setq flag 1)                       ;; To keep track of the current direction of boat (flag = 1 means from start shore to end shore and vice versa)
    (loop
        (cond 
            ((= flag -1)       

                (sortMin nlist)
                (setq parent (car nlist))
                (setf tt (subseq parent 0 2))
                (print tt)
                ;; (append result tt)
                (cond 
                    ;;Check if goalstate reached
                    ((not (null (goalstate tt nl)))
                        (format t "GoalState")
                        ;; (print result)
                        (print "-------------------------------")
                        (print "Total No. of Steps Taken to Reach The Goal")
                        (print count)
                        (print "-------------------------------")
                        (print "Total No. of Nodes Expanded")
                        (print expandedNodes)
                        (return )
                    )     
                )

                (setq vnlist (append vnlist (list tt)))

                ;;Generate the Successors of current node
                (setq childs (findchilds parent flag))
                (setq temp nil)
                (setq count (+ 1 count))

                ;; Iterate over childs and add to the queue if not visited yet
                (dolist (ch childs)
                    (setf tm (subseq ch 0 2))
                    (cond 
                        ((null (position tm vplist :test #'equal))
                            (setq temp (append temp (list ch)))
                            (setq expandedNodes (+ expandedNodes 1))
                        )
                    )
                )
                ;; This condition can occur only if the current state is goal state.
                (cond 
                    ((= (list-length childs) 0)
                        (format t "Goal-State")
                        (return ) 
                    )
                )
                (setq plist temp)
                (setq nlist (cdr nlist))
            )
            ((= flag 1)
                (sortMin plist)
                (setq parent (car plist))
                (setf tt (subseq parent 0 2))

                (print tt)
                ;; (append result tt)

                (cond 
                    ((not (null (goalstate tt nl)))
                        (print tt)
                        (return )
                    )     
                )
               

                (setq vplist (append vplist (list tt)))
                (setq childs (findchilds parent flag))
                (setq count (+ 1 count))
                (setq temp nil)
                (dolist (ch childs)
                    (setf tm (subseq ch 0 2))
                    (cond 
                        ((null (position tm vnlist :test #'equal))
                            (setq temp (append temp (list ch)))
                            (setq expandedNodes (+ expandedNodes 1))
                        )
                    )
                )
                (setq nlist temp)
                (setq plist (cdr plist))
            )
        )
        (setq flag (* -1 flag))
    )
)