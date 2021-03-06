(defclass gl-canvas :super gl::glviewsurface
	:slots (cmap topleft bottomright moving prevpos motion button))

(defmethod gl-canvas
  (:create (&rest args)
     (send-super* :create args)
     (setq cmap (gethash (send self :colormap) x:*xwindows-hash-tab*))
     (setq prevpos #i(0 0))
     self)
  (:buttonpress (evnt)
     (setq topleft (x:event-pos evnt)
	   bottomright (copy-seq topleft))
     (send x::parent :select-model
	 (aref topleft 0) (aref topleft 1) (x:event-state evnt))
     (setq prevpos topleft)
     )
  (:motionNotify (evnt)
 ;;    (send-super :motionNotify evnt) ; don't draw selection rectangle
;;   (format t "glcanvas motionNotify~%")
     (setq moving t)
     (setq x::bottomright (x:event-pos evnt))
     (setq motion (v- x::bottomright prevpos))
     (setq prevpos  x::bottomright)
     (send x::parent
	   (cond ((x:event-left evnt) :move-xy)
		 ((x:event-middle evnt) :move-zz)
		 ((x:event-right evnt) :move-rot) )
	   (elt motion 0) (- (elt motion 1)) )
     )
  (:buttonrelease (evnt)
     (send-super :buttonrelease evnt)
     (setq moving nil)
  )
 (:enterNotify (evnt)  ;;     (send cmap :install)
	)
 (:leaveNotify (evnt)  ;;     (send cmap :uninstall)
	) )
