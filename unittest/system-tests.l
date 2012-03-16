(deftest sha-224 ()
  (values
   (si:sha-224 "")
   (si:sha-224 "The quick brown fox jumps over the lazy dog")
   (si:sha-224 "The quick brown fox jumps over the lazy dog."))
  => "d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f"
  => "730e109bd7a8a32b1cb9d9a09aa2325d2430587ddbc0c38bad911525"
  => "619cba8e8e05826e9b8c519c0a5c68f4fb653e8a3d8aa04bb2c8cd4c")

(deftest sha-256 ()
  (values
   (si:sha-256 "")
   (si:sha-256 "The quick brown fox jumps over the lazy dog")
   (si:sha-256 "The quick brown fox jumps over the lazy dog."))
  => "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  => "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592"
  => "ef537f25c895bfa782526529a9b63d97aa631564d5d789c2b765448c8635fb6c")

(deftest sha-384 ()
  (values
   (si:sha-384 "")
   (si:sha-384 "The quick brown fox jumps over the lazy dog")
   (si:sha-384 "The quick brown fox jumps over the lazy dog."))
  => "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b"
  => "ca737f1014a48f4c0b6dd43cb177b0afd9e5169367544c494011e3317dbf9a509cb1e5dc1e85a941bbee3d7f2afbc9b1"
  => "ed892481d8272ca6df370bf706e4d7bc1b5739fa2177aae6c50e946678718fc67a7af2819a021c2fc34e91bdb63409d7")

(deftest sha-512 ()
  (values
   (si:sha-512 "")
   (si:sha-512 "The quick brown fox jumps over the lazy dog")
   (si:sha-512 "The quick brown fox jumps over the lazy dog."))
  => "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"
  => "07e547d9586f6a73f73fbac0435ed76951218fb7d0c8d788a309d785436bbb642e93a252a954f23912547d1e8a3b5ed6e1bfd7097821233fa0538f3db854fee6"
  => "91ea1245f20d46ae9a037a989f54f1f790f0a47607eeb8a14d12890cea77a1bbc6c7ed9cf205e67b7f2b8fd4c7dfd3a7a8617e45f3c463d481c7e586c39ac1ed")
