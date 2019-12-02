func foo(outer: String) {

  let tmp1 = 100
  var tmp2 = 500
  let tmp3 = Int(100)
  let tmp4: Int? = 100
  let tmp5: Int = tmp4!

  if tmp1 == 100 && tmp2 == 500 {
    let tmp6: Int = tmp4!
   }

  bar(tmp1)

}

func bar(_ inner: Int) -> Int {
    let a = inner 
    return a
}
