package prosomo

import scala.concurrent.duration._
import com.typesafe.config.{Config, ConfigFactory}
import java.io.File

import io.iohk.iodb.ByteArrayWrapper

import collection.JavaConverters._

trait Parameters extends Utils {

  //tags for identifying ledger entries
  val forgeBytes = ByteArrayWrapper("FORGER_REWARD".getBytes)
  val genesisBytes = ByteArrayWrapper("GENESIS".getBytes)

  def getConfig:Config = {
    import Prosomo.input
    if (input.length > 0) {
      val inputConfigFile = new File(input.head.stripSuffix(".conf")+".conf")
      val localConfig = ConfigFactory.parseFile(inputConfigFile).getConfig("input")
      val baseConfig = ConfigFactory.load
      if (input.length == 2) {
        val lineConfig = ConfigFactory.parseString(input(1)).getConfig("input")
        ConfigFactory.load(lineConfig.withFallback(localConfig)).withFallback(baseConfig)
      } else {
        ConfigFactory.load(localConfig).withFallback(baseConfig)
      }
    } else {
      val baseConfig = ConfigFactory.load
      val localConfig = ConfigFactory.load("local")
      localConfig.withFallback(baseConfig)
    }
  }

  val config:Config = getConfig

  val inputCommands:Map[Int,List[String]] = if (config.hasPath("command")) {
    var out:Map[Int,List[String]] = Map()
    val cmdList = config.getStringList("command.cmd").asScala.toList
    for (line<-cmdList) {
      val com = line.trim.split(" ")
      com(0) match {
        case s:String => {
          if (com.length == 2){
            com(1).toInt match {
              case i:Int => {
                if (out.keySet.contains(i)) {
                  val nl = s::out(i)
                  out -= i
                  out += (i->nl)
                } else {
                  out += (i->List(s))
                }
              }
              case _ =>
            }
          }
        }
        case _ =>
      }
    }
    out
  } else {
    Map()
  }
  //use network delay parameterization if true
  val useDelayParam:Boolean = config.getBoolean("params.useDelayParam")
  //number of stakeholders
  val numHolders:Int = config.getInt("params.numHolders")
  //duration of slot in milliseconds
  val slotT:Long = config.getInt("params.slotT")
  //delay in milliseconds per kilometer in router model
  val delay_ms_km:Double = config.getDouble("params.delay_ms_km")
  //delay in milliseconds per bit in router model
  val delay_ms_byte:Double = config.getDouble("params.delay_ms_byte")
  //network random noise max
  val delay_ms_noise:Double = config.getDouble("params.delay_ms_noise")
  //communication method
  val useRouting:Boolean = config.getBoolean("params.useRouting")
  //delay in slots, calculated as maximum possible delay in random global network model
  val delta_s:Int = (40075.0*delay_ms_km/slotT+1.0).ceil.toInt
  //epoch parameter
  val epsilon_s:Double = config.getDouble("params.epsilon_s")
  //alert stake ratio
  val alpha_s:Double = config.getDouble("params.alpha_s")
  //participating stake ratio
  val beta_s:Double = config.getDouble("params.beta_s")
  //active slot coefficient
  val f_s:Double = if (useDelayParam) {
    val out = 1.0-math.exp(1.0/(delta_s+1.0))*(1+epsilon_s)/(2.0*alpha_s)
    assert(out>0)
    out
  } else {
    config.getDouble("params.f_s")
  }
  // checkpoint depth in slots, k parameter in maxValid-bg, k > 192*delta/epsilon*beta
  val k_s:Int = if(useDelayParam) {
    (192.0*delta_s/(epsilon_s*beta_s)).floor.toInt + 1
  } else {
    config.getInt("params.k_s")
  }
  // epoch length R >= 3k/2f
  val epochLength:Int = if (useDelayParam) {
    3*(k_s*(0.5/f_s)).toInt
  } else {
    config.getInt("params.epochLength")
  }
  // slot window for chain selection, s = k/4f
  val slotWindow:Int = if (useDelayParam) {
    (k_s*0.25/f_s).toInt
  } else {
    config.getInt("params.slotWindow")
  }
  //simulation runtime in slots
  val L_s:Int = config.getInt("params.L_s")
  //status and verify check chain hash data up to this depth to gauge consensus amongst actors
  val confirmationDepth:Int = config.getInt("params.confirmationDepth")
  //max initial stake
  val initStakeMax:Double = config.getDouble("params.initStakeMax")
  //max random transaction delta
  val maxTransfer:Double = config.getDouble("params.maxTransfer")
  //reward for forging blocks
  val forgerReward:Double = config.getDouble("params.forgerReward")
  //percent of transaction amount taken as fee by the forger
  val transactionFee:Double = config.getDouble("params.transactionFee")
  //number of holders on gossip list for sending new blocks and transactions
  val numGossipers:Int = config.getInt("params.numGossipers")
  //use gossiper protocol
  val useGossipProtocol:Boolean = config.getBoolean("params.useGossipProtocol")
  //max number of tries for a tine to ask for parent blocks
  val tineMaxTries:Int = config.getInt("params.tineMaxTries")
  //max depth in multiples of confirmation depth that can be returned from an actor
  val tineMaxDepth:Int = config.getInt("params.tineMaxDepth")
  //data write interval in slots
  val dataOutInterval:Int = epochLength
  //time out for dropped messages from coordinator
  val waitTime:FiniteDuration = config.getInt("params.waitTime") seconds
  //duration between update tics that stakeholder actors send to themselves
  val updateTime:FiniteDuration = config.getInt("params.updateTime") millis
  //duration between command read tics and transaction generation for the coordinator
  val commandUpdateTime:FiniteDuration = config.getInt("params.commandUpdateTime") millis
  //number of txs per block
  val txPerBlock:Int = config.getInt("params.txPerBlock")
  //max number of transactions to be issued over lifetime of simulation
  val txMax:Int = config.getInt("params.txMax")
  //Issue random transactions if true
  var transactionFlag:Boolean = config.getBoolean("params.transactionFlag")
  // p = txProbability => (1-p)^numHolders
  var txProbability:Double = config.getDouble("params.txProbability")
  //uses randomness for public key seed and initial stake, set to false for deterministic run
  val randomFlag:Boolean = config.getBoolean("params.randomFlag")
  //when true, if system cpu load is too high the coordinator will stall to allow stakeholders to catch up
  val performanceFlag:Boolean = config.getBoolean("params.performanceFlag")
  //threshold of cpu usage above which coordinator will stall if performanceFlag = true
  val systemLoadThreshold:Double = config.getDouble("params.systemLoadThreshold")
  //number of values to average for load threshold
  val numAverageLoad:Int = config.getInt("params.numAverageLoad")
  //print Stakeholder 0 status per slot if true
  val printFlag:Boolean = config.getBoolean("params.printFlag")
  //print Stakeholder 0 execution time per slot if true
  val timingFlag:Boolean = config.getBoolean("params.timingFlag")
  //Record data if true, plot data points with ./cmd.sh and enter command: plot
  val dataOutFlag:Boolean = config.getBoolean("params.dataOutFlag")
  //path for data output files
  val dataFileDir:String = config.getString("params.dataFileDir")
  //toggle for action based round execution
  val useFencing = config.getBoolean("params.useFencing")
  //seed for pseudo random runs
  val inputSeed:String = {
    if (randomFlag) {
      uuid
    } else {
      config.getString("params.inputSeed")
    }
  }
  val stakeDistribution:String = config.getString("params.stakeDistribution")
  val stakeScale:Double = config.getDouble("params.stakeScale")
  val initStakeMin:Double = config.getDouble("params.initStakeMin")
}
