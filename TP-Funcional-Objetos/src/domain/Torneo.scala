package domain 

case class Torneo(
		  postas:   List[Posta],
		  participantes: List[ParticipanteTorneo],
		  dragones: List[Dragon],
		  reglas : Reglas
		)
{
  
  def competir : Option[ParticipanteTorneo] = {
    reglas.decidirGanador(jugarPostas)
  }
  
  def jugarPostas : List[ParticipanteTorneo] = {
    postas.foldLeft(participantes){(participantesEnJuego,posta) =>
      if(hayMasDeUnVikingo(participantesEnJuego) )
        jugarPosta(participantesEnJuego, posta).map(_.reOrganizate)
      else
        participantesEnJuego
    }
  }
  
  def hayMasDeUnVikingo(participantes: List[ParticipanteTorneo]) = participantes.size > 1
  
  def jugarPosta(participantesEnJuego: List[ParticipanteTorneo],posta:Posta) : List[ParticipanteTorneo] = {
    val participantesMontadosONo = reglas.eleccionDeDragones(participantesEnJuego,posta,dragones)
    val vikingosListos = prepararParticipantes(participantesMontadosONo).flatten
    val ganadores = posta.participar(vikingosListos).map(_.vikingo)
    reglas.quienesAvanzan(ganadores)    
  }
  
  def prepararParticipantes(participantes: List[ParticipanteTorneo]) = { participantes map {
    case vikingoOJinete:ParticipantePosta => List() :+ vikingoOJinete
    case equipo:Equipo => equipo.vikingos
    }    
  }
  
}

  

sealed trait EstadoTorneo
case class  EnJuego(participantes : List[ParticipanteTorneo]) extends EstadoTorneo
case class  Ganador(participante: ParticipanteTorneo) extends EstadoTorneo
case object NoHayGanador extends EstadoTorneo


