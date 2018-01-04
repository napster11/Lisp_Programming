;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Shivam Singh (5414019)
;; Course: CSCI-5511
;; Instructore: Nikos Papanikolopoulos
;; Problem: 8 Puzzle Problem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check if current state is a goal state or not
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun goalState(node)
    (setf goal '((1 2 3) (4 5 6) (7 8 E)))
    (equal node goal)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check if Starting state is an Infeasible one or not
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun infeasibleCheck(node)
    (setq cnt 0)
    (setq ls (first node))
    (setq ls (append ls (second node)))
    (setq ls (append ls (third node)))
    (setq len (list-length ls))
    

    ;; Count the total number of Inversions on a board and 
    ;; If the inversion count is odd then board will lead to an infeasible state
    ;; Reference: http://www.geeksforgeeks.org/check-instance-8-puzzle-solvable/

    (loop for i from 0 to (- len 2)
    do
        (loop for j from (+ 1 i) to (- len 1) 
        do
            (if (and (not (eq (nth i ls) 'E)) (not (eq (nth j ls) 'E)))
                (cond 
                    ((< (nth j ls) (nth i ls) )
                        (setq cnt (+ 1 cnt))
                    )
                )    
            )
        )
    )
    (rem cnt 2)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To Print the order in which nodes were traversed to reach to the goal state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun printPath(node)


    ;; A node is in the form of Node: ((...)(...)(...)(g h) (parent of current Node))
    ;; Try to move from top to bottom by passing fifth attribute of a node to next 
    ;; Recursive call till we reach to the initial state with parent equals to null
    (if (null (fifth node)) (return )(setf fifthN (fifth node)))
    
    (cond
        ((not (null fifthN))
            (setq temp (copy-list fifthN))
            (setq temp (list (first temp) (second temp) (third temp)))
            
            (if (not (null (car temp))) (print temp))
            (if (not (null (car temp))) (printPath fifthN))
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculate the heuristic value of current Board. H = Total No. of misplaced elements on Board
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun heuristic(node)
    
    (setf goal '((1 2 3) (4 5 6) (7 8 E)))
    (setf count 0)
    (setf firstN  (first  node))
    (setf secondN (second node))
    (setf thirdN  (third  node))
    (setf firstG  (first  goal))
    (setf secondG (second goal))
    (setf thirdG  (third  goal))


    ;; Iterate the first list of 2D list board and try to find out the position of E 
    ;; Plus compare with Goal State to find out the total No. of misplaced elements
    (loop for x in firstN
            for y in firstG
                do (if (and (not (eq x 'E)) (not (eq y 'E))) (if (= x y) (setf count (+ 1 count))))
    )

    ;; Iterate the second list of 2D list board and try to find out the position of E 
    ;; Plus compare with Goal State to find out the total No. of misplaced elements
    (loop for x in secondN
            for y in secondG
                do (if (and (not (eq x 'E)) (not (eq y 'E))) (if (= x y) (setf count (+ 1 count))))
    )

    ;; Iterate the third list of 2D list board and try to find out the position of E 
    ;; Plus compare with Goal State to find out the total No. of misplaced elements
    (loop for x in thirdN
            for y in thirdG
                do (if (and (not (eq x 'E)) (not (eq y 'E))) (if (= x y) (setf count (+ 1 count))))
    )

    (setf cnt (- 8 count))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find the successor of the current Board
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun findChilds(parent)
    (setf result nil)

    (setf first  (first parent))
    (setf second (second parent))
    (setf third  (third parent))
    (setf g (car (fourth parent)))
    (setf h (car (cdr (fourth parent))))
   

    ;; Check If E is in the first List
    (if (not (null (find 'E third)))
        (cond
            ;; If E is int first column of first Row
            ((eq (car(cdr (cdr third))) 'E) 
                (setq upper (copy-list second)) 
                (setq current (copy-list third))
                (rotatef (nth 2 upper) (nth 2 current)) 
                (setq current1 (copy-list third))
                (rotatef (nth 2 current1) (nth 1 current1)) 

                ;; Store the all possible list in the variables
                (setq newList1 (list first upper current ))
                (setq newList2 (list first second current1))

                ;; Append pair of (g h) at the end of list
                (setq newlist1 (append newlist1 (list (list (+ 1 g) (heuristic newlist1)) parent)))
                (setq newlist2 (append newlist2 (list (list (+ 1 g) (heuristic newlist2)) parent)))
                (setf result (list newList1 newList2 ))

            )
            ;; If E is in the second column of first Row
            ((eq (car(cdr third)) 'E) 
                (setq upper (copy-list second)) 
                (setq current (copy-list third)) 
                (rotatef (nth 1 upper) (nth 1 current))
                (setq current1 (copy-list third)) 
                (setq current2 (copy-list third))
                (rotatef (nth 1 current1) (nth 2 current1)) 
                (rotatef (nth 0 current2) (nth 1 current2))

                ;; Store the all possible list in the variables
                (setq newList1 (list first upper current )) 
                (setq newList2 (list first second current1))
                (setq newList3 (list first second current2)) 

                ;; Append pair of (g h) at the end of list
                (setq newlist1 (append newlist1 (list (list (+ 1 g) (heuristic newlist1)) parent)))
                (setq newlist2 (append newlist2 (list (list (+ 1 g) (heuristic newlist2)) parent)))
                (setq newlist3 (append newlist3 (list (list (+ 1 g) (heuristic newlist3)) parent)))

                (setf result (list newList1 newList2 newList3 ))
            )

            ;; If E is in the third column of first Row
            ((eq (car third) 'E) 
                (setq upper (copy-list second)) 
                (setq current (copy-list third)) 
                (rotatef (nth 0 upper) (nth 0 current))
                (setq current1 (copy-list third)) 
                (rotatef (nth 1 current1) (nth 0 current1))

                ;; Store the all possible list in the variables
                (setq newList1 (list first upper current )) 
                (setq newList2 (list first second current1))

                ;; Append pair of (g h) at the end of list
                (setq newlist1 (append newlist1 (list (list (+ 1 g) (heuristic newlist1)) parent)))
                (setq newlist2 (append newlist2 (list (list (+ 1 g) (heuristic newlist2)) parent)))
                (setf result (list newList1 newList2))
            )
        )  
    )  

    ;; Check If E is in the Second List
    (if (not (null (find 'E second)))
        (cond
            ;; If E is in first column of second Row
            ((eq (car(cdr (cdr second))) 'E) 
                (setq upper (copy-list first)) 
                (setq current (copy-list second))
                (setq down  (copy-list third))
                (rotatef (nth 2 upper) (nth 2 current))
                (setq current1 (copy-list second))
                (rotatef (nth 2 current1) (nth 1 current1))
                (setq current2 (copy-list second))
                (rotatef (nth 2 current2) (nth 2 down))

                ;; Store the all possible list in the variables
                (setq newList1 (list upper current third))
                (setq newList2 (list first current1 third))
                (setq newList3 (list first current2 down)) 

                ;; Append pair of (g h) at the end of list
                (setq newlist1 (append newlist1 (list (list (+ 1 g) (heuristic newlist1)) parent)))
                (setq newlist2 (append newlist2 (list (list (+ 1 g) (heuristic newlist2)) parent)))
                (setq newlist3 (append newlist3 (list (list (+ 1 g) (heuristic newlist3)) parent)))

                (setf result (list newList1 newList2 newList3 ))
            )
            ;; If E is in second column of second Row
            ((eq (car(cdr second)) 'E) 
                (setq upper (copy-list first)) 
                (setq current (copy-list second))
                (setq down  (copy-list third))
                (rotatef (nth 1 upper) (nth 1 current))
                (setq current1 (copy-list second))
                (rotatef (nth 2 current1) (nth 1 current1))
                (setq current2 (copy-list second))
                (rotatef (nth 1 current2) (nth 0 current2))
                (setq current3 (copy-list second))
                (rotatef (nth 1 current3) (nth 1 down))

                ;; Store the all possible list in the variables
                (setq newList1 (list upper current third))
                (setq newList2 (list first current1 third))
                (setq newList3 (list first current2 third))
                (setq newList4 (list first current3 down))

                ;; Append pair of (g h) at the end of list
                (setq newlist1 (append newlist1 (list (list (+ 1 g) (heuristic newlist1)) parent)))
                (setq newlist2 (append newlist2 (list (list (+ 1 g) (heuristic newlist2)) parent)))
                (setq newlist3 (append newlist3 (list (list (+ 1 g) (heuristic newlist3)) parent)))
                (setq newlist4 (append newlist4 (list (list (+ 1 g) (heuristic newlist4)) parent)))

                (setf result (list newList1 newList2 newList3 newList4 ))
            )

            ;; If E is in third column of second Row
            ((eq (car second) 'E) 
                (setq upper (copy-list first)) 
                (setq current (copy-list second))
                (setq down  (copy-list third))
                (rotatef (nth 0 upper) (nth 0 current))
                (setq current1 (copy-list second))
                (rotatef (nth 1 current1) (nth 0 current1))
                (setq current2 (copy-list second))
                (rotatef (nth 0 current2) (nth 0 down))

                ;; Store the all possible list in the variables
                (setq newList1 (list upper current third))
                (setq newList2 (list first current1 third))
                (setq newList3 (list first current2 down)) 

                ;; Append pair of (g h) at the end of list
                (setq newlist1 (append newlist1 (list (list (+ 1 g) (heuristic newlist1)) parent)))
                (setq newlist2 (append newlist2 (list (list (+ 1 g) (heuristic newlist2)) parent)))
                (setq newlist3 (append newlist3 (list (list (+ 1 g) (heuristic newlist3)) parent)))

                (setf result (list newList1 newList2 newList3 ))
            )
        )
    )


    ;; Check If E is in the third Row
    (if (not (null (find 'E first)))
        (cond
            ;; If E is in first column of third Row
            ((eq (car(cdr (cdr first))) 'E) 
                (setq current (copy-list first))
                (setq down  (copy-list second))
                (rotatef (nth 2 current) (nth 2 down))
                (setq current1 (copy-list first))
                (rotatef (nth 2 current1) (nth 1 current1))

                ;; Store the all possible list in the variables
                (setq newList1 (list current down third))
                (setq newList2 (list current1 second third))

                ;; Append pair of (g h) at the end of list
                (setq newlist1 (append newlist1 (list (list (+ 1 g) (heuristic newlist1)) parent)))
                (setq newlist2 (append newlist2 (list (list (+ 1 g) (heuristic newlist2)) parent)))

                (setf result (list newList1 newList2))
            )

            ;; If E is in second column of third Row
            ((eq (car(cdr first)) 'E) 
                (setq current (copy-list first))
                (setq down  (copy-list second))
                (rotatef (nth 1 current) (nth 1 down))
                (setq current1 (copy-list first))
                (rotatef (nth 2 current1) (nth 1 current1))
                (setq current2 (copy-list first))
                (rotatef (nth 1 current2) (nth 0 current2))

                ;; Store the all possible list in the variables
                (setq newList1 (list current down third))
                (setq newList2 (list current1 second third))
                (setq newList3 (list current2 second third))

                ;; Append pair of (g h) at the end of list
                (setq newlist1 (append newlist1 (list (list (+ 1 g) (heuristic newlist1)) parent)))
                (setq newlist2 (append newlist2 (list (list (+ 1 g) (heuristic newlist2)) parent)))
                (setq newlist3 (append newlist3 (list (list (+ 1 g) (heuristic newlist3)) parent)))


                (setf result (list newList1 newList2 newList3))
            )

            ;; If E is in third column of third Row
            ((eq (car first) 'E) 
                (setq current (copy-list first))
                (setq down  (copy-list second))
                (rotatef (nth 0 current) (nth 0 down))
                (setq current1 (copy-list first))
                (rotatef (nth 1 current1) (nth 0 current1))

                ;; Store the all possible list in the variables
                (setq newList1 (list current down third))
                (setq newList2 (list current1 second third))

                ;; Append pair of (g h) at the end of list
                (setq newlist1 (append newlist1 (list (list (+ 1 g) (heuristic newlist1)) parent)))
                (setq newlist2 (append newlist2 (list (list (+ 1 g) (heuristic newlist2)) parent)))
                
                (setf result (list newList1 newList2))
            )
        ) 
    )  
    (setq result result)

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sort the list in increasing order based on the (g+h) value
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sortMin(ls)
    (sort ls #'(lambda (a b) (< (+ (car (cdr (car (nthcdr 3 a)))) (car (car (nthcdr 3 a)))) (+ (car (cdr (car (nthcdr 3 b)))) (car (car (nthcdr 3 b)))))))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A* algorithm to reach the Goal State using given intital state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun start (l)

    ;; Create two list nlist and vlist to store expanded and visited nodes respectively

    (setq count 0)
    (setq steps 0)
    (setq vlist nil)                ;; Visited List
    
    (Setq l (append l (list '(0 0))))
    (Setq l (append l (list '(nil))))
    (setq nlist (append (list l)))  ;; Node List
    (setq vlist (append (list '((-1 -1 -1) (-1 -1 -1) (-1 -1 -1)))))


    ;; Iterate Until Goal State Reached
    (loop 

        ;; Check if Start state will lead to infeasible solution or not
        (cond 
            (( = (infeasibleCheck l) 1)
                (format t "Infeasible Puzzle" #\return)
                (return )
            ) 
            (( = (list-length nlist) 0)
                (return )
            )
        )

        ;;Sort the list and take out the node with minimum G+H value
        (sortMin nlist)
        (setq parent (car nlist))
        (setf tt (subseq parent 0 3))
        ;; (print tt)

        ;; Terminate If Goal State Reached
        (cond 
            ((not (null (goalstate tt)))
                ;; (print tt)
                (print "-------------------------------------")
                (print "Path Generated")
                (print tt)
                (printPath parent)
                
                (print "Total No. of Nodes Expanded")
                (print (+ (list-length nlist) (list-length vlist)))
                (print " Total Steps Taken")
                (print steps)
                
                (return )
            )      
        )

        (setq vlist (append vlist (list tt)))

        

        ;;Count the no. of expanded Nodes
        (setq steps (+ steps 1))

        ;; Generate the successor of current state
        (setq childs (findChilds parent))

        ;;Iterate over successors and add to the list if not visited yet.
        (print "*******************************************")
        (print "New States Generated")
        (print "*******************************************")
        (print childs)
        
        (setq cnt 0)
        (dolist (ch childs)
                (setf tm (subseq ch 0 3)) 
                (cond 
                    ((null (position tm vlist :test #'equal))
                        (setq nlist (append nlist (list ch)))
                        (setq cnt (+ cnt 1))
                    )
                )
        )
        (setq nlist (cdr nlist))   
    )
)

