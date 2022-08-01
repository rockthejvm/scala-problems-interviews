package com.rockthejvm.strings

import scala.annotation.tailrec

object Justify {

  def justify(text: String, width: Int): String = ???

  def main(args: Array[String]): Unit = {
    println(justify("Scala is the most amazing language you will ever write any code in", 6))
    println(justify("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam vel urna bibendum, pharetra mi quis, imperdiet nibh. Praesent dictum odio lacus, eget commodo sem aliquam rutrum. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec eget rhoncus mauris, quis vehicula mi. Quisque finibus purus non varius dictum. Pellentesque vulputate fringilla egestas. Nunc eleifend ex sed egestas cursus. Praesent molestie nisl in pretium vehicula. Vestibulum efficitur ut risus quis porta. Praesent non sem quam. Donec vitae arcu sapien. Quisque aliquet nibh in metus efficitur ullamcorper. Donec mattis dapibus nisl sed iaculis. Curabitur eu blandit enim. Fusce varius.", 80))
    println(justify("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc vestibulum gravida justo, id luctus nulla dapibus ut. Ut elementum ac metus at vestibulum. Quisque pellentesque id nisi sed efficitur. Maecenas consectetur diam ac orci convallis mollis. Etiam sem purus, accumsan consequat mattis at, gravida at justo. Nullam molestie ex non cursus semper. Cras porta nunc sed tempus finibus. Maecenas pretium nibh est, id scelerisque turpis convallis vel. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Aliquam erat volutpat. Duis sed convallis augue. Integer lacus eros, posuere ut metus at, vestibulum consectetur lorem.\n\nPhasellus vel pulvinar sem, a rutrum lectus. Donec quis nisi nec leo faucibus sodales id non nunc. Fusce gravida diam vitae orci sollicitudin, a efficitur urna fermentum. Vivamus gravida ante sed lectus ornare, eget egestas leo porta. Curabitur sed blandit metus. Ut at augue consequat, suscipit nulla eu, euismod tellus. Mauris eu mi faucibus, elementum ligula sed, placerat lectus. Morbi varius magna eu mauris ultricies, vitae blandit purus pharetra. Etiam finibus non odio in eleifend. Cras viverra, dolor at ullamcorper pharetra, ligula nunc mattis est, ut volutpat odio dui vitae ligula. Nulla vitae tincidunt arcu. Nulla facilisi. Duis ullamcorper eros neque, at tincidunt.", 100))
  }
}
