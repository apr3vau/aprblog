sbcl --eval "(asdf:load-system :aprblog)" \
     --eval "(setf uiop:*image-entry-point* #'aprblog:main)" \
     --eval "(uiop:dump-image \"aprblog\" :executable t :compression t)"
