package u03

enum Person :
  case Student ( name : String , year : Int )
  case Teacher ( name : String , course : String )

object Person :
  def name ( p: Person ): String = p match 
    case Student (n , _) => n
    case Teacher (n , _) => n
