package com.rockthejvm.strings

object RansomNote {

  /*
     ransomNote(
      "I have your daughter. I want 1000000 dollars, or you'll never see her again.",
      "I bought this really nice doll for my daughter. It was 20 dollars on Amazon. She's never been happier.
       I often have discounts from my network, so if you want to buy some really cool stuff for your kids, I can send you an invite
       if you sign up to my newsletter. It's read by 100000 people, and you'll never need to search for online discounts again."
     )
   */
  def ransomNote(note: String, magazine: String): Boolean = ???

  def main(args: Array[String]): Unit = {
    println(
      ransomNote(
        "I have your daughter. I want 1000000 dollars, or you'll never see her again.",
        "I bought this really nice doll for my daughter. It was 20 dollars on Amazon. She's never been happier. I often have discounts from my network, so if you want to buy some really cool stuff for your kids, I can send you an invite if you sign up to my newsletter. It's read by 100000 people, and you'll never need to search for online discounts again."
      )
    )
  }

}
