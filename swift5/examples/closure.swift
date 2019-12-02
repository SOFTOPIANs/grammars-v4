func foo(htmlText: String) {

    var sum: (_ s1:Int, _ s2:Int) -> Int = {  
    let tmp = 100
    return s1 + $1 + tmp
    }  
    
    var res = sum(1,2)

    let a = 1
    let b = 2

  let closureSum: () -> Int = {
     a+b
  }

  closureSum()

 }

// TODO: Add more complicated and autoclosures