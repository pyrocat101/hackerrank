BufferedReader br = new BufferedReader(new InputStreamReader(System.in))
for (_ in 1..(br.readLine().toInteger())) {
    br.readLine()
    println (br.readLine().split().inject(1) { acc, it -> acc * it.toInteger() % 1234567 })
}
