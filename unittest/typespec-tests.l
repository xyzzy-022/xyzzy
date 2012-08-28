(deftest wait-object-typep ()
  (values
   (typep (si:*create-wait-object) 'wait-object)
   (typep nil 'wait-object)
   (typep 0 'wait-object)
   (typep "" 'wait-object))
  => t
  => nil
  => nil
  => nil)

(deftest wait-object-check-type-1 ()
  (check-type (si:*create-wait-object) wait-object)
  => nil)

(deftest wait-object-check-type-2 ()
  (check-type "" wait-object)
  !! type-error)


(deftest char-encoding-typep ()
  (values
   (typep *encoding-sjis* 'char-encoding)
   (typep nil 'char-encoding)
   (typep 0 'char-encoding)
   (typep "" 'char-encoding))
  => t
  => nil
  => nil
  => nil)

(deftest char-encoding-check-type-1 ()
  (check-type *encoding-sjis* char-encoding)
  => nil)

(deftest char-encoding-check-type-2 ()
  (check-type "" char-encoding)
  !! type-error)


(deftest structure-definition-typep ()
  (values
   (typep (defstruct structure-definition-typep)
          'si:structure-definition)
   (typep nil 'si:structure-definition)
   (typep 0 'si:structure-definition)
   (typep "" 'si:structure-definition))
  => t
  => nil
  => nil
  => nil)

(deftest structure-definition-check-type-1 ()
  (check-type (defstruct structure-definition-check-type-1)
              si:structure-definition)
  => nil)

(deftest structure-definition-check-type-2 ()
  (check-type "" si:structure-definition)
  !! type-error)


(deftest dll-module-p ()
  (values
   (si:*dll-module-p (si:load-dll-module "msvcrt"))
   (si:*dll-module-p nil)
   (si:*dll-module-p 0)
   (si:*dll-module-p ""))
  => t
  => nil
  => nil
  => nil)

(deftest dll-module-typep ()
  (values
   (typep (si:load-dll-module "msvcrt")
          'si:dll-module)
   (typep nil 'si:dll-module)
   (typep 0 'si:dll-module)
   (typep "" 'si:dll-module))
  => t
  => nil
  => nil
  => nil)

(deftest dll-module-check-type-1 ()
  (check-type (si:load-dll-module "msvcrt")
              si:dll-module)
  => nil)

(deftest dll-module-check-type-2 ()
  (check-type "" si:dll-module)
  !! type-error)


(deftest c-function-typep ()
  (values
   (typep (si:make-c-function (si:load-dll-module "msvcrt")
                              "atoi" '(:int32) :int32)
          'si:c-function)
   (typep nil 'si:c-function)
   (typep 0 'si:c-function)
   (typep "" 'si:c-function))
  => t
  => nil
  => nil
  => nil)

(deftest c-function-check-type-1 ()
  (check-type (si:make-c-function (si:load-dll-module "msvcrt")
                                  "atoi" '(:int32) :int32)
              si:c-function)
  => nil)

(deftest c-function-check-type-2 ()
  (check-type "" si:c-function)
  !! type-error)


(deftest c-callable-typep ()
  (values
   (typep (si:make-c-callable #'car :void :void)
          'si:c-callable)
   (typep nil 'si:c-callable)
   (typep 0 'si:c-callable)
   (typep "" 'si:c-callable))
  => t
  => nil
  => nil
  => nil)

(deftest c-callable-check-type-1 ()
  (check-type (si:make-c-callable #'car :void :void)
              si:c-callable)
  => nil)

(deftest c-callable-check-type-2 ()
  (check-type "" si:c-callable)
  !! type-error)


(deftest environment-typep ()
  (values
   (typep (macrolet ((env (&environment env)
                       env))
            (env))
          'si:environment)
   (typep nil 'si:environment)
   (typep 0 'si:environment)
   (typep "" 'si:environment))
  => t
  => nil
  => nil
  => nil)

(deftest environment-check-type-1 ()
  (check-type (macrolet ((env (&environment env)
                           env))
                (env))
              si:environment)
  => nil)

(deftest environment-check-type-2 ()
  (check-type "" si:environment)
  !! type-error)
