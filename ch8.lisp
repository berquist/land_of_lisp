(load "graph-util")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
  "Choose a random node number between 1 and the maximum number of
node, inclusive."
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  "Make an undirected edge between two nodes."
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  "A list of edges is a list of pairs of integers."
  (apply #'append (loop repeat *edge-num*
                     collect (edge-pair (random-node) (random-node)))))

(defparameter *ctci-edges*
  '((0 . 1)
    (1 . 2)
    (2 . 0)
    (2 . 3)
    (3 . 2)
    (4 . 6)
    (5 . 4)
    (6 . 5)))
(defparameter *ctci-nodes*
  (remove-duplicates (union (mapcar #'car *ctci-edges*)
                            (mapcar #'cdr *ctci-edges*))))

(defun direct-edges (node edge-list)
  "Given a node number and an edge list, return all edges directly
connecting the given node to other nodes."
  (remove-if-not (lambda (x)
                   (eql (car x) node))
                 edge-list))

(defun get-connected (node edge-list)
  "Perform in-order traversal to find all nodes connected to the given
node."
  (let ((visited nil))
    (labels ((traverse (node)
               (unless (member node visited)
                 (push node visited)
                 (mapc (lambda (edge)
                         (traverse (cdr edge)))
                       (direct-edges node edge-list)))))
      (traverse node))
    visited))

(defun find-islands (nodes edge-list)
  "Return a list of node lists, each corresponding to nodes that are
connected to each other, but each node list is an island."
  (let ((islands nil))
    (labels ((find-island (nodes)
               (let* ((connected (get-connected (car nodes) edge-list))
                      (unconnected (set-difference nodes connected)))
                 (push connected islands)
                 (when unconnected
                   (find-island unconnected)))))
      (find-island nodes))
    islands))

(defun connect-with-bridges (islands)
  "Given a list of islands, return edges that connect the first node
of an island with the first node of the next island."
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  "Add edges that connect islands onto the original edge list."
  (append (connect-with-bridges (find-islands nodes edge-list))
          edge-list))

(defun make-city-edges ()
  ""
  (let* ((nodes (loop for i from 1 to *node-num*
                   collect 1))
         (edge-list (connect-all-islands nodes (make-edge-list)))
         (cops (remove-if-not (lambda (x)
                                (zerop (random *cop-odds*)))
                              edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

(defun edges-to-alist (edge-list)
  "Turn a list of edges (cons pairs) into an association list.

Example:
'((0 . 1) (1 . 2) (2 . 0) (2 . 3) (3 . 2) (4 . 6) (5 . 4) (6 . 5))
becomes
'((0 (1)) (1 (2)) (2 (0) (3)) (3 (2)) (4 (6)) (5 (4)) (6 (5)))
"
  (mapcar (lambda (node1)
            (cons node1
                  (mapcar (lambda (edge)
                            (list (cdr edge)))
                          (remove-duplicates (direct-edges node1 edge-list)
                                             :test #'equal))))
          (remove-duplicates (mapcar #'car edge-list))))

(defun add-cops (edge-alist edges-with-cops)
  "

`edges-with-cops` shoulbe a list of cons pairs, not an alist."
  (mapcar (lambda (x)
            (let ((node1 (car x))
                  (node1-edges (cdr x)))
              (cons node1
                    (mapcar (lambda (edge)
                              (let ((node2 (car edge)))
                                (if (intersection (edge-pair node1 node2)
                                                  edges-with-cops
                                                  :test #'equal)
                                    (list node2 'cops)
                                    edge)))
                            node1-edges))))
          edge-alist))

(assert (equal
         (add-cops
          '((0 (1)) (1 (2)) (2 (0) (3)) (3 (2)) (4 (6)) (5 (4)) (6 (5)))
          '((3 . 2)))
         '((0 (1)) (1 (2)) (2 (0) (3 COPS)) (3 (2 COPS)) (4 (6)) (5 (4)) (6 (5)))))

(defun neighbors (node edge-alist)
  "Make the list of nodes that are immedate (one away) neigbors of the
given node."
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (a b edge-alist)
  "Are nodes a and b within one (neighbors) of each other?"
  (member b (neighbors a edge-alist)))

(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x)
              (within-one x b edge-alist))
            (neighbors a edge-alist))))

(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
        (glow-worms (loop for i below *worm-num*
                         collect (random-node))))
    (loop for n from 1 to *node-num*
       collect (append (list n)
                       (cond ((eql n wumpus) '(wumpus))
                             ((within-two n wumpus edge-alist) '(blood!)))
                       (cond ((member n glow-worms) '(glow-worm))
                             ((some (lambda (worm)
                                      (within-one n worm edge-alist))
                                    glow-worms)
                              '(lights!)))
                       ;; This works because (cdr (assoc ...)) will be
                       ;; a list of single-element lists, and (cdr
                       ;; (cdr (assoc ...))) will only be non-nil when
                       ;; 'cops are there.
                       (when (some #'cdr (cdr (assoc n edge-alist)))
                         '(sirens!))))))

(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
        (find-empty-node)
        x)))

(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city))

;; CL-USER> *congestion-city-nodes*
;; ((1 LIGHTS!) (2 LIGHTS! SIRENS!) (3 BLOOD! LIGHTS!) (4 SIRENS!) (5 GLOW-WORM) (6 SIRENS!) (7 SIRENS!) (8
;; BLOOD! SIRENS!) (9) (10 SIRENS!)  (11 BLOOD! SIRENS!) (12) (13 BLOOD!
;; SIRENS!) (14 SIRENS!)  (15 WUMPUS LIGHTS! SIRENS!) (16) (17 BLOOD!)
;; (18 SIRENS!) (19 LIGHTS! SIRENS!)  (20 SIRENS!) (21 SIRENS!) (22
;; BLOOD! GLOW-WORM) (23 BLOOD! LIGHTS!) (24) (25 BLOOD!) (26 LIGHTS!)
;; (27) (28 GLOW-WORM) (29 SIRENS!) (30 BLOOD! LIGHTS!))
;; CL-USER> *player-pos*
;; 16
;; CL-USER> *congestion-city-edges*
;; ((29 (6 COPS)) (23 (22)) (4 (20 COPS)) (20 (10 COPS) (4 COPS)) (16 (1) (26))
;;  (22 (23) (15)) (15 (22) (11 COPS)) (1 (16) (21) (5))
;;  (14 (12) (8) (7 COPS) (2 COPS)) (8 (10) (14) (24) (11 COPS))
;;  (6 (29 COPS) (18 COPS)) (25 (18) (11) (7)) (30 (11) (26) (5))
;;  (18 (25) (6 COPS) (21)) (10 (20 COPS) (8) (19)) (7 (14 COPS) (25) (27))
;;  (27 (7)) (26 (30) (16) (28)) (28 (2) (26)) (2 (14 COPS) (28) (17))
;;  (12 (14) (13)) (24 (8) (3)) (5 (1) (3) (30) (19))
;;  (11 (3) (30) (25) (15 COPS) (13 COPS) (8 COPS) (17)) (17 (2) (11) (19))
;;  (19 (21 COPS) (10) (5) (17) (3)) (3 (11) (5) (24) (19))
;;  (13 (11 COPS) (12) (21)) (21 (1) (19 COPS) (18) (13)))

(defun ingredients (order)
  (mapcan (lambda (burger)
            (case burger
              (single '(patty))
              (double '(patty patty))
              (double-cheese '(patty patty cheese))))
          order))
(ingredients '(single double-cheese double))

(defun known-city-nodes ()
  ""
  (mapcar (lambda (node)
            (if (member node *visited-nodes*)
                ;; if visited and present, mark with *, otherwise do
                ;; nothing
                (let ((n (assoc node *congestion-city-nodes*)))
                  (if (eql node *player-pos*)
                      (append n '(*))
                      n))
                ;; mark unvisited nodes with ?
                (list node '?)))
          ;; for each node in the list of visited nodes, get the nodes
          ;; connected to it, append them all together, then remove
          ;; the duplicates
          (remove-duplicates
           (append *visited-nodes*
                   (mapcan (lambda (node)
                             ;; get the nodes connected to this node
                             (mapcar #'car
                                     (cdr (assoc node
                                                 *congestion-city-edges*))))
                           *visited-nodes*)))))

(defun known-city-edges ()
  (mapcar (lambda (node)
            (cons node (mapcar (lambda (x)
                                 ;; if the node has been visited, add
                                 ;; it, otherwise TODO
                                 (if (member (car x) *visited-nodes*)
                                     x
                                     (list (car x))))
                               (cdr (assoc node *congestion-city-edges*)))))
            *visited-nodes*))

(defun draw-known-city ()
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))

(defun walk (pos)
  (handle-direction pos nil))

(defun charge (pos)
  (handle-direction pos t))

(defun handle-direction (pos charging)
  (let ((edge (assoc pos
                     (cdr (assoc *player-pos* *congestion-city-edges*)))))
    (if edge
        (handle-new-place edge pos charging)
        (princ "That location does not exist!"))))

(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*))
         (has-worm (and (member 'glow-worm node)
                        (not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond ((member 'cops edge) (princ "You ran into the cops. Game Over."))
          ((member 'wumpus node) (if charging
                                     (princ "You found the Wumpus!")
                                     (princ "You ran into the Wumpus")))
          (charging (princ "You wasted your last bullet. Game Over."))
          (has-worm (let ((new-pos (random-node)))
                      (princ "You ran into a Glow Worm Gang! You're now at ")
                      (princ new-pos)
                      (handle-new-place nil new-pos nil))))))

;; This is the map from the book.
(defparameter *book-congestion-city-nodes*
  '((8 blood! sirens!) (6 blood! sirens!) (23) (12 blood!) (13) (16 blood! sirens!) (14 blood! sirens!) (9 blood! lights!) (1 blood! glow-worm sirens!) (25 wumpus lights! sirens!) (30 sirens!) (11 blood! lights!) (21 blood! lights!) (22) (15) (7) (5 blood! sirens!) (26 blood!) (17 blood! lights!) (29 sirens!) (10 sirens!) (19 blood! glow-worm) (18 sirens!) (4 blood! lights! sirens!) (3 sirens!) (28 sirens!) (2) (20 lights!) (27 glow-worm) (24)))
(defparameter *book-congestion-city-edges*
  '())
