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
