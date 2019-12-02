func foo() {
    var arr0: [Int]
    arr[0] = 100
    bar(arr[0])

    var arr1 = [Int]()
    arr1[0][1] = 100    
    bar(arr1[8])

    let val = 10;
    
    var arr2 = [5,val]
    
    var arr3: [Int] = []

    var arr4 = Array(repeating: 0, count: 10)

    var arr5 = Array(arrayLiteral:1, 2, 3)
    var arr6 = Array(1...3)
}
 
func bar(_ arrayLiteral: Int) {
    print(arrayLiteral)
}
