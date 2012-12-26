(deftest *do-completion-matched-list-order ()
  (*do-completion "1" :list nil '("100" "12" "123" "222" "111" "333" "11"))
  => "1"
  => ("11" "111" "123" "12" "100")
  => nil)
