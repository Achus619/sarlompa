

package object domain {

  type RequisitoMontura = (Vikingo,Dragon) => Boolean
  val vanidoso: RequisitoMontura = (vikingo,dragon) => dragon.danio > vikingo.danio
  val tiene: Item => RequisitoMontura = item => (vikingo,_) => vikingo.item == item
  val pesado: Int => RequisitoMontura = peso => (vikingo,_) => vikingo.peso < peso
  
  type RequisitoVeto = (Dragon) => Boolean
  val masDeCienDeAtaque: RequisitoVeto = dragon => dragon.danio > 100
  
  
  val Hacha   = Arma(30,0,10)
  val Maza    = Arma(100,0,40)
  val SistemaDeVuelo   = Arma(2,30,5)
  val ComestibleHambre = Consumible(4)
  val Rojo = Equipo(List(Patan: Vikingo, Patapez))
  
  val Hipo    = Vikingo(item = SistemaDeVuelo, equipo = None)
  val Astrid  = Vikingo(item = Hacha, equipo = None)
  val Patan   = Vikingo(item = Maza, equipo = None)
  val Patapez = Vikingo(item = ComestibleHambre, efectos = EfectosPosta(50,2), equipo = None)
  
  
  
  
  val Chimuelo = FuriaNocturna(100,List(tiene(SistemaDeVuelo)))
  val Slifer   = NadderMortifero(List(vanidoso))
  val DragonPesado = Gronckle(1000,100, List()) //no deberia pasarle esto
  
  val participantesInvierno = List(Hipo,Astrid,Patan,Patapez)
  val dragones = List(Slifer,Chimuelo,DragonPesado)  
  
  //val FestivalDeInvierno = Torneo(List(Pesca,Combate,Carrera),participantesInvierno)
      
}

