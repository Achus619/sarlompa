package domain

import scala.util.Try
import scala.collection.JavaConversions.asScalaBuffer

abstract class Reglas { 

  def eleccionDeDragones(vikingos:List[Vikingo],posta:Posta,dragones:List[Dragon]) = {
    vikingos.foldLeft((List(): List[ParticipantePosta],dragones)){ (participantesYDragones,vikingo) =>
       elegirFormaDeJugar(participantesYDragones._1,participantesYDragones._2,vikingo,posta)
    }._1
  }
  
  def elegirFormaDeJugar(participantesEnJuego:List[ParticipantePosta],dragonesDisponibles:List[Dragon],vikingo:Vikingo,posta:Posta) = {
    val mejorJinete = vikingo.mejorMontura(dragonesDisponibles, posta)
    if(esMejorSinMontura(vikingo,mejorJinete,posta))
      participaComoVikingo(participantesEnJuego, dragonesDisponibles, vikingo)
    else
      participaComoJinete(participantesEnJuego, dragonesDisponibles, mejorJinete.get)
  }
  
  def esMejorSinMontura(vikingo: Vikingo, jinete: Option[Jinete], posta: Posta) =
      jinete.isEmpty || vikingo.esMejorQue(jinete.get, posta) 
  
  def participaComoVikingo(participantesEnJuego : List[ParticipantePosta], dragonesDisponibles: List[Dragon], vikingo : Vikingo) =
    (actualizarParticipantes(vikingo, participantesEnJuego),dragonesDisponibles)
   
  def participaComoJinete(participantesEnJuego : List[ParticipantePosta], dragonesDisponibles: List[Dragon], jinete : Jinete) =
    (actualizarParticipantes(jinete, participantesEnJuego),actualizarDragones(jinete.dragon,dragonesDisponibles))
      
  def actualizarParticipantes(participante: ParticipantePosta, participantesEnJuego : List[ParticipantePosta]) =
    participantesEnJuego :+ participante
      
  def actualizarDragones(dragonASacar: Dragon, dragonesDisponibles: List[Dragon]) =
    dragonesDisponibles.filter(_ != dragonASacar)
  
  
	def quienesAvanzan(vikingos: List[Vikingo]) : List[Vikingo]
  
	def decidirGanador(participantes: List[Vikingo]) : Option[ParticipanteTorneo]
}

class Estandar extends Reglas{
  
  def decidirGanador(participantes: List[Vikingo]) : Option[ParticipanteTorneo] = 
    participantes.headOption
  
  
  def quienesAvanzan(vikingos: List[Vikingo]) =
    vikingos.take(laMitad(vikingos))
    
  def laMitad(vikingos: List[Vikingo]) = vikingos.size/2
  
}
case class Eliminacion(siguen:Int) extends Estandar{
  override def quienesAvanzan(vikingos: List[Vikingo]) =
    vikingos.take(siguen)
    
}
case object Inverso extends Estandar{
  
  override def quienesAvanzan(vikingos: List[Vikingo]) =
    vikingos.takeRight(laMitad(vikingos))
    
  override def decidirGanador(participantes:List[Vikingo]) =
    super.decidirGanador(participantes.reverse)
  
}
case class Veto(condicion : RequisitoVeto ) extends Estandar{
  
  override def eleccionDeDragones(vikingos : List[Vikingo], posta : Posta, dragones: List[Dragon]): List[ParticipantePosta] = 
    super.eleccionDeDragones(vikingos,posta,restringirDragones(dragones))
  
  def restringirDragones(dragones: List[Dragon]) : List[Dragon] =
    dragones.filter(dragon => condicion.apply(dragon))
  
}
case object Handicap extends Estandar{
  
  override def eleccionDeDragones(vikingos : List[Vikingo], posta : Posta, dragones: List[Dragon]): List[ParticipantePosta] = 
   super.eleccionDeDragones(vikingos.reverse, posta,dragones).reverse
  
}

case object Equipos extends Estandar{
  
 override def decidirGanador(participantes:List[Vikingo]) : Option[ParticipanteTorneo] = 
     participantes.groupBy(_.equipo).mapValues(_.size).maxBy(_._2)._1
 
 
 
}











