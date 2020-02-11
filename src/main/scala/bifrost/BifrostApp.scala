package bifrost

/*

* @startuml

* car --|> wheel

* @enduml

*/

import akka.actor.{ActorRef, Props}
import bifrost.api.http._
import bifrost.blocks.BifrostBlock
import bifrost.forging.{Forger, ForgingSettings}
import bifrost.history.BifrostSyncInfoMessageSpec
import bifrost.network.BifrostNodeViewSynchronizer
import bifrost.scorexMod.GenericApplication
import bifrost.transaction.box.BifrostBox
import io.circe
import bifrost.api.http.{ApiRoute, UtilsApiRoute}
import bifrost.network.message.MessageSpec
import bifrost.transaction.box.proposition.ProofOfKnowledgeProposition
import bifrost.transaction.state.PrivateKey25519
import java.lang.management.ManagementFactory

import bifrost.transaction.bifrostTransaction.BifrostTransaction
import com.sun.management.HotSpotDiagnosticMXBean

import scala.reflect.runtime.universe._

class BifrostApp(val settingsFilename: String) extends GenericApplication with Runnable {

  override type P = ProofOfKnowledgeProposition[PrivateKey25519]
  override type BX = BifrostBox
  override type TX = BifrostTransaction
  override type PMOD = BifrostBlock
  override type NVHT = BifrostNodeViewHolder

  implicit lazy val settings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }
  log.debug(s"Starting application with settings \n$settings")

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] =
    Seq(BifrostSyncInfoMessageSpec)

  override val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new NVHT(settings)))

  val forger: ActorRef = actorSystem.actorOf(Props(classOf[Forger], settings, nodeViewHolderRef))

  override val localInterface: ActorRef = actorSystem.actorOf(
    Props(classOf[BifrostLocalInterface], nodeViewHolderRef, forger, settings)
  )

  override val nodeViewSynchronizer: ActorRef = actorSystem.actorOf(
    Props(classOf[BifrostNodeViewSynchronizer],
      networkController,
      nodeViewHolderRef,
      localInterface,
      BifrostSyncInfoMessageSpec)
  )

  override val apiRoutes: Seq[ApiRoute] = Seq(
    DebugApiRoute(settings, nodeViewHolderRef),
    WalletApiRoute(settings, nodeViewHolderRef),
    ProgramApiRoute(settings, nodeViewHolderRef, networkController),
    AssetApiRoute(settings, nodeViewHolderRef),
    UtilsApiRoute(settings),
    NodeViewApiRoute(settings, nodeViewHolderRef)
  )

  override val apiTypes: Seq[Type] = Seq(typeOf[UtilsApiRoute],
                                         typeOf[DebugApiRoute],
                                         typeOf[WalletApiRoute],
                                         typeOf[ProgramApiRoute],
                                         typeOf[AssetApiRoute],
                                         typeOf[NodeViewApiRoute])


         val vm_version = System.getProperty("java.vm.version")
         System.out.printf("java.vm.version = %s%n", vm_version)

         val bean = ManagementFactory.getPlatformMXBean(classOf[HotSpotDiagnosticMXBean])

          val enableJVMCI = bean.getVMOption("EnableJVMCI")
          System.out.println(enableJVMCI)

          val useJVMCICompiler = bean.getVMOption("UseJVMCICompiler")
          System.out.println(useJVMCICompiler)

         val compiler = System.getProperty("jvmci.Compiler")
         System.out.printf("jvmci.Compiler = %s%n", compiler)


  forger
  localInterface
  nodeViewSynchronizer

}

object BifrostApp extends App {
  val settingsFilename = args.headOption.getOrElse("testnet-private.json")
  new BifrostApp(settingsFilename).run()
}