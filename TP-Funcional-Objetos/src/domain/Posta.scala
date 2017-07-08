package domain

  trait Posta{
	  
    def participar(participantes :List[ParticipantePosta]) : List[ParticipantePosta] =
      aplicarEfectos(empezarPosta(participantes))
	  
	  
	  def aplicarEfectos(participantes:List[ParticipantePosta]) = 
	    for{participante <- participantes} yield aplicarEfecto(participante)
	  
	  
    def empezarPosta[T <: ParticipantePosta](participantes: List[T]) =
      for {
        participante <- ordenarPorMejor(participantes)  if puedeParticipar(participante) 
      }
      yield participante
      
    def ordenarPorMejor[T <: ParticipantePosta](participantes : List[T]) = 
       participantes.sortWith((unParticipante,otroParticipante) => unParticipante.esMejorQue(otroParticipante, this))
      
    
    def puedeParticipar[T <: ParticipantePosta](participante : T  ) = 
      cumpleCondicion(participante) && noVaAEstarHambriento(participante)
      
    def noVaAEstarHambriento[T <: ParticipantePosta](participante:T) =
      !aplicarEfecto(participante).estaHambriento
      
    def aplicarEfecto(participante: ParticipantePosta) = participante.aplicarEfecto(hambreLuegoDePosta).terminarPosta
    
    def cumpleCondicion(participante: ParticipantePosta)   : Boolean
	  def esMejorQue(mejor:ParticipantePosta,peor:ParticipantePosta) : Boolean
    def hambreLuegoDePosta : Int
      
}
  case class Pesca(pesoMin : Int)     extends Posta {
    
   def cumpleCondicion(participante: ParticipantePosta ) = participante.peso > pesoMin
   
   def hambreLuegoDePosta = 5
   
   def esMejorQue(mejor: ParticipantePosta, peor: ParticipantePosta) = mejor.capacidadDeCarga > peor.capacidadDeCarga 
   
  }
  case class Combate(barbarosidadMin: Int = 0)      extends Posta {
    
    def cumpleCondicion(participante: ParticipantePosta) = barbarosidadMayorAlMinimo(participante) || tieneUnArma(participante)
    
    def barbarosidadMayorAlMinimo(participante: ParticipantePosta) = participante.barbarosidad > barbarosidadMin
    
    def tieneUnArma(participante: ParticipantePosta) = participante.vikingo.item match {
      case Arma(_,_,_)   => true
      case _             => false
    }
    
    def hambreLuegoDePosta = 10
    
    def esMejorQue(mejor: ParticipantePosta, peor: ParticipantePosta) = mejor.danio > peor.danio
    
  }
  case class Carrera(kms: Int) extends Posta {
    
    def cumpleCondicion(participante: ParticipantePosta) = participante match {
      case Jinete(_,_) => true
      case _           => false
    }
    
    def hambreLuegoDePosta = kms
    
    def esMejorQue(mejor: ParticipantePosta, peor: ParticipantePosta) = mejor.velocidad > peor.velocidad
    
  }