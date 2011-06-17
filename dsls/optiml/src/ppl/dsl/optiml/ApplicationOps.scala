package ppl.dsl.optiml

import java.io.{PrintWriter}
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._
import ppl.delite.framework.DSLType
import ppl.dsl.optiml.datastruct.scala._

/**
 * This file should be auto-generated!
 */

trait ApplicationOps extends BinarizedGradientPyramidOps with RectOps with BiGGDetectionOps with BinarizedGradientTemplateOps
  with DenoiseVertexDataOps with DenoiseEdgeDataOps with APairOps with PQOps with AClusterOps
trait ApplicationOpsExp extends BinarizedGradientPyramidOpsExp with RectOpsExp with BiGGDetectionOpsExp with BinarizedGradientTemplateOpsExp
  with DenoiseVertexDataOpsExp with DenoiseEdgeDataOpsExp with APairOpsExp with PQOpsExp with AClusterOpsExp {
  this: OptiMLExp =>
}
trait ScalaGenApplicationOps extends ScalaGenBinarizedGradientPyramidOps with ScalaGenRectOps with ScalaGenBiGGDetectionOps with ScalaGenBinarizedGradientTemplateOps
  with ScalaGenDenoiseVertexDataOps with ScalaGenDenoiseEdgeDataOps with ScalaGenAPairOps with ScalaGenPQOps with ScalaGenAClusterOps

trait DenoiseVertexDataOps extends DSLType with Variables {
  object DenoiseVertexData {
    def apply(id: Rep[Int], b: Rep[Vector[Double]], p: Rep[Vector[Double]]) = denoise_vertex_data_obj_new(id, b, p)
  }

  implicit def repDenoiseVertexDataToDenoiseVertexDataOps(v: Rep[DenoiseVertexData]) = new denoiseVertexDataOpsCls(v)

  class denoiseVertexDataOpsCls(v: Rep[DenoiseVertexData]) {
    def id = denoise_vertex_data_id(v)
    def belief = denoise_vertex_data_belief(v)
    def setBelief(b: Rep[Vector[Double]]) = denoise_vertex_data_belief_update(v, b)
    def potential = denoise_vertex_data_potential(v)
  }

  // object defs
  def denoise_vertex_data_obj_new(id: Rep[Int], b: Rep[Vector[Double]], p: Rep[Vector[Double]]): Rep[DenoiseVertexData]

  // class defs
  def denoise_vertex_data_id(v: Rep[DenoiseVertexData]): Rep[Int]
  def denoise_vertex_data_belief(v: Rep[DenoiseVertexData]): Rep[Vector[Double]]
  def denoise_vertex_data_belief_update(v: Rep[DenoiseVertexData], b: Rep[Vector[Double]])
  def denoise_vertex_data_potential(v: Rep[DenoiseVertexData]): Rep[Vector[Double]]
}

trait DenoiseVertexDataOpsExp extends DenoiseVertexDataOps with EffectExp {
  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class DenoiseVertexDataObjectNew(id: Exp[Int], belief: Exp[Vector[Double]], potential: Exp[Vector[Double]])
    extends Def[DenoiseVertexData] {
    val vD = manifest[DenoiseVertexDataImpl]
  }
  case class DenoiseVertexDataId(v: Exp[DenoiseVertexData]) extends Def[Int]
  case class DenoiseVertexDataBelief(v: Exp[DenoiseVertexData]) extends Def[Vector[Double]]
  case class DenoiseVertexDataBeliefUpdate(v: Exp[DenoiseVertexData], b: Exp[Vector[Double]]) extends Def[Unit]
  case class DenoiseVertexDataPotential(v: Exp[DenoiseVertexData]) extends Def[Vector[Double]]

  /////////////////////
  // object interface
  def denoise_vertex_data_obj_new(id: Exp[Int], b: Exp[Vector[Double]], p: Exp[Vector[Double]]) = reflectEffect(DenoiseVertexDataObjectNew(id, /*reflectRead*/(b), /*reflectRead*/(p)))

  /////////////////////
  // class interface

  def denoise_vertex_data_id(v: Exp[DenoiseVertexData]) = DenoiseVertexDataId(/*reflectRead*/(v))
  def denoise_vertex_data_belief(v: Exp[DenoiseVertexData]) = DenoiseVertexDataBelief(/*reflectRead*/(v))
  def denoise_vertex_data_belief_update(v: Exp[DenoiseVertexData], b: Exp[Vector[Double]]) = reflectWrite(v)(DenoiseVertexDataBeliefUpdate(/*reflectWrite*/(v), b))
  def denoise_vertex_data_potential(v: Exp[DenoiseVertexData]) = DenoiseVertexDataPotential(/*reflectRead*/(v))
}

trait ScalaGenDenoiseVertexDataOps extends ScalaGenBase {
  val IR: ApplicationOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case v@DenoiseVertexDataObjectNew(id,b,p) => emitValDef(sym, "new " + remap(v.vD) + "(" + quote(id) + "," + quote(b) + "," + quote(p) + ")")
      case DenoiseVertexDataId(v) => emitValDef(sym, quote(v) + ".id")
      case DenoiseVertexDataBelief(v) => emitValDef(sym, quote(v) + ".belief")
      case DenoiseVertexDataBeliefUpdate(v,b) => emitValDef(sym, quote(v) + ".setBelief(" + quote(b) + ")")
      case DenoiseVertexDataPotential(v) => emitValDef(sym, quote(v) + ".potential")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait DenoiseEdgeDataOps extends DSLType with Variables {
  object DenoiseEdgeData {
    def apply(m: Rep[Vector[Double]], oM: Rep[Vector[Double]]) = denoise_edge_data_obj_new(m, oM)
  }

  implicit def repDenoiseEdgeDataToDenoiseEdgeDataOps(e: Rep[DenoiseEdgeData]) = new denoiseEdgeDataOpsCls(e)

  class denoiseEdgeDataOpsCls(e: Rep[DenoiseEdgeData]) {
    def message = denoise_edge_data_message(e)
    def setMessage(m: Rep[Vector[Double]]) = denoise_edge_data_message_update(e,m)
    def oldMessage = denoise_edge_data_old_message(e)
    def setOldMessage(m: Rep[Vector[Double]]) = denoise_edge_data_old_message_update(e,m)
    def cloneL = denoise_edge_data_cloneL(e)
  }

  // object defs
  def denoise_edge_data_obj_new(m: Rep[Vector[Double]], oM: Rep[Vector[Double]]): Rep[DenoiseEdgeData]

  // class defs
  def denoise_edge_data_message(e: Rep[DenoiseEdgeData]): Rep[Vector[Double]]
  def denoise_edge_data_message_update(e: Rep[DenoiseEdgeData], m: Rep[Vector[Double]])
  def denoise_edge_data_old_message(e: Rep[DenoiseEdgeData]): Rep[Vector[Double]]
  def denoise_edge_data_old_message_update(e: Rep[DenoiseEdgeData], m: Rep[Vector[Double]])
  def denoise_edge_data_cloneL(e: Rep[DenoiseEdgeData]): Rep[DenoiseEdgeData]
}

trait DenoiseEdgeDataOpsExp extends DenoiseEdgeDataOps with EffectExp {
  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class DenoiseEdgeDataObjectNew(m: Exp[Vector[Double]], oM: Exp[Vector[Double]])
    extends Def[DenoiseEdgeData] {
    val eD = manifest[DenoiseEdgeDataImpl]
  }
  case class DenoiseEdgeDataMessage(e: Exp[DenoiseEdgeData]) extends Def[Vector[Double]]
  case class DenoiseEdgeDataMessageUpdate(e: Exp[DenoiseEdgeData], m: Exp[Vector[Double]]) extends Def[Unit]
  case class DenoiseEdgeDataOldMessage(e: Exp[DenoiseEdgeData]) extends Def[Vector[Double]]
  case class DenoiseEdgeDataOldMessageUpdate(e: Exp[DenoiseEdgeData], m: Exp[Vector[Double]]) extends Def[Unit]
  case class DenoiseEdgeDataCloneL(e: Exp[DenoiseEdgeData]) extends Def[DenoiseEdgeData]

  /////////////////////
  // object interface

  def denoise_edge_data_obj_new(m: Exp[Vector[Double]], oM: Exp[Vector[Double]]) = reflectEffect(DenoiseEdgeDataObjectNew(m, oM))

  /////////////////////
  // class interface

  def denoise_edge_data_message(e: Exp[DenoiseEdgeData]) = DenoiseEdgeDataMessage(/*reflectRead*/(e))
  def denoise_edge_data_message_update(e: Exp[DenoiseEdgeData], m: Exp[Vector[Double]]) = reflectWrite(e)(DenoiseEdgeDataMessageUpdate(/*reflectWrite*/(e), m))
  def denoise_edge_data_old_message(e: Exp[DenoiseEdgeData]) = DenoiseEdgeDataOldMessage(/*reflectRead*/(e))
  def denoise_edge_data_old_message_update(e: Exp[DenoiseEdgeData], m: Exp[Vector[Double]]) = reflectWrite(e)(DenoiseEdgeDataOldMessageUpdate(/*reflectWrite*/(e), m))
  def denoise_edge_data_cloneL(e: Exp[DenoiseEdgeData]) = reflectEffect(DenoiseEdgeDataCloneL(/*reflectRead*/(e)))
}

trait ScalaGenDenoiseEdgeDataOps extends ScalaGenBase {
  val IR: ApplicationOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case e@DenoiseEdgeDataObjectNew(m,oM) => emitValDef(sym, "new " + remap(e.eD) + "(" + quote(m) + "," + quote(oM) + ")")
      case DenoiseEdgeDataMessage(e) => emitValDef(sym, quote(e) + ".message")
      case DenoiseEdgeDataMessageUpdate(e,m) => emitValDef(sym, quote(e) + ".setMessage(" + quote(m) + ")")
      case DenoiseEdgeDataOldMessage(e) => emitValDef(sym, quote(e) + ".oldMessage")
      case DenoiseEdgeDataOldMessageUpdate(e,m) => emitValDef(sym, quote(e) + ".setOldMessage(" + quote(m) + ")")
      case DenoiseEdgeDataCloneL(e) => emitValDef(sym, quote(e) + ".cloneL")
      case _ => super.emitNode(sym, rhs)
    }
  }
}







trait BiGGDetectionOps extends DSLType with Variables with OverloadHack {

  object BiGGDetection {
    def apply(name: Rep[String], score: Rep[Float], roi: Rep[Rect], mask: Rep[GrayscaleImage], index: Rep[Int], x: Rep[Int], y: Rep[Int], tpl: Rep[BinarizedGradientTemplate], crt_tpl: Rep[BinarizedGradientTemplate]) = biggdetection_obj_new(name, score, roi, mask, index, x, y, tpl, crt_tpl)
  }

  implicit def repBiGGDetectionToBiGGDetectionOps(x: Rep[BiGGDetection]) = new biggdetectionOpsCls(x)
  implicit def biggdetectionToBiGGDetectionOps(x: Var[BiGGDetection]) = new biggdetectionOpsCls(readVar(x))

  class biggdetectionOpsCls(__x: Rep[BiGGDetection]) {
    def name = biggdetection_name(__x)
    def score = biggdetection_score(__x)
    def roi = biggdetection_roi(__x)
    def mask = biggdetection_mask(__x)
    def index = biggdetection_index(__x)
    def x = biggdetection_x(__x)
    def y = biggdetection_y(__x)
    def tpl = biggdetection_tpl(__x)
    def crt_tpl = biggdetection_crt_tpl(__x)
  }

  //object defs
  def biggdetection_obj_new(name: Rep[String], score: Rep[Float], roi: Rep[Rect], mask: Rep[GrayscaleImage], index: Rep[Int], x: Rep[Int], y: Rep[Int], tpl: Rep[BinarizedGradientTemplate], crt_tpl: Rep[BinarizedGradientTemplate]): Rep[BiGGDetection]

  //class defs
  def biggdetection_name(__x: Rep[BiGGDetection]): Rep[String]
  def biggdetection_score(__x: Rep[BiGGDetection]): Rep[Float]
  def biggdetection_roi(__x: Rep[BiGGDetection]): Rep[Rect]
  def biggdetection_mask(__x: Rep[BiGGDetection]): Rep[GrayscaleImage]
  def biggdetection_index(__x: Rep[BiGGDetection]): Rep[Int]
  def biggdetection_x(__x: Rep[BiGGDetection]): Rep[Int]
  def biggdetection_y(__x: Rep[BiGGDetection]): Rep[Int]
  def biggdetection_tpl(__x: Rep[BiGGDetection]): Rep[BinarizedGradientTemplate]
  def biggdetection_crt_tpl(__x: Rep[BiGGDetection]): Rep[BinarizedGradientTemplate]
}

trait BiGGDetectionOpsExp extends BiGGDetectionOps with EffectExp {
  case class BiGGDetectionObjectNew(name: Exp[String], score: Exp[Float], roi: Exp[Rect], mask: Exp[GrayscaleImage], index: Exp[Int], x: Exp[Int], y: Exp[Int], tpl: Exp[BinarizedGradientTemplate], crt_tpl: Exp[BinarizedGradientTemplate]) extends Def[BiGGDetection]
  case class BiGGDetectionName(__x: Exp[BiGGDetection]) extends Def[String]
  case class BiGGDetectionScore(__x: Exp[BiGGDetection]) extends Def[Float]
  case class BiGGDetectionRoi(__x: Exp[BiGGDetection]) extends Def[Rect]
  case class BiGGDetectionMask(__x: Exp[BiGGDetection]) extends Def[GrayscaleImage]
  case class BiGGDetectionIndex(__x: Exp[BiGGDetection]) extends Def[Int]
  case class BiGGDetectionX(__x: Exp[BiGGDetection]) extends Def[Int]
  case class BiGGDetectionY(__x: Exp[BiGGDetection]) extends Def[Int]
  case class BiGGDetectionTpl(__x: Exp[BiGGDetection]) extends Def[BinarizedGradientTemplate]
  case class BiGGDetectionCrt_tpl(__x: Exp[BiGGDetection]) extends Def[BinarizedGradientTemplate]

  def biggdetection_obj_new(name: Exp[String], score: Exp[Float], roi: Exp[Rect], mask: Exp[GrayscaleImage], index: Exp[Int], x: Exp[Int], y: Exp[Int], tpl: Exp[BinarizedGradientTemplate], crt_tpl: Exp[BinarizedGradientTemplate]) = reflectEffect(BiGGDetectionObjectNew(name, score, roi, mask, index, x, y, tpl, crt_tpl))
  def biggdetection_name(__x: Rep[BiGGDetection]) = BiGGDetectionName(__x)
  def biggdetection_score(__x: Rep[BiGGDetection]) = BiGGDetectionScore(__x)
  def biggdetection_roi(__x: Rep[BiGGDetection]) = BiGGDetectionRoi(__x)
  def biggdetection_mask(__x: Rep[BiGGDetection]) = BiGGDetectionMask(__x)
  def biggdetection_index(__x: Rep[BiGGDetection]) = BiGGDetectionIndex(__x)
  def biggdetection_x(__x: Rep[BiGGDetection]) = BiGGDetectionX(__x)
  def biggdetection_y(__x: Rep[BiGGDetection]) = BiGGDetectionY(__x)
  def biggdetection_tpl(__x: Rep[BiGGDetection]) = BiGGDetectionTpl(__x)
  def biggdetection_crt_tpl(__x: Rep[BiGGDetection]) = BiGGDetectionCrt_tpl(__x)
}

trait ScalaGenBiGGDetectionOps extends ScalaGenBase {
  val IR: ApplicationOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  // these are the ops that call through to the underlying real data structure
    case BiGGDetectionObjectNew(name, score, roi, mask, index, x, y, tpl, crt_tpl) => emitValDef(sym, "new " + remap(manifest[BiGGDetection]) + "(" + quote(name)  + "," + quote(score)  + "," + quote(roi)  + "," + quote(mask)  + "," + quote(index)  + "," + quote(x)  + "," + quote(y)  + "," + quote(tpl)  + "," + quote(crt_tpl)  + ")")
    case BiGGDetectionName(x) =>  emitValDef(sym, quote(x) + ".name")
    case BiGGDetectionScore(x) =>  emitValDef(sym, quote(x) + ".score")
    case BiGGDetectionRoi(x) =>  emitValDef(sym, quote(x) + ".roi")
    case BiGGDetectionMask(x) =>  emitValDef(sym, quote(x) + ".mask")
    case BiGGDetectionIndex(x) =>  emitValDef(sym, quote(x) + ".index")
    case BiGGDetectionX(x) =>  emitValDef(sym, quote(x) + ".x")
    case BiGGDetectionY(x) =>  emitValDef(sym, quote(x) + ".y")
    case BiGGDetectionTpl(x) =>  emitValDef(sym, quote(x) + ".tpl")
    case BiGGDetectionCrt_tpl(x) =>  emitValDef(sym, quote(x) + ".crt_tpl")
    case _ => super.emitNode(sym, rhs)
  }
}

trait BinarizedGradientPyramidOps extends DSLType with Variables with OverloadHack {

  object BinarizedGradientPyramid {
    def apply(pyramid: Rep[Vector[GrayscaleImage]], start_level: Rep[Int], levels: Rep[Int], fixedLevelIndex: Rep[Int]) = binarizedgradientpyramid_obj_new(pyramid, start_level, levels, fixedLevelIndex)
  }

  implicit def repBinarizedGradientPyramidToBinarizedGradientPyramidOps(x: Rep[BinarizedGradientPyramid]) = new binarizedgradientpyramidOpsCls(x)
  implicit def binarizedgradientpyramidToBinarizedGradientPyramidOps(x: Var[BinarizedGradientPyramid]) = new binarizedgradientpyramidOpsCls(readVar(x))

  class binarizedgradientpyramidOpsCls(__x: Rep[BinarizedGradientPyramid]) {
    def pyramid = binarizedgradientpyramid_pyramid(__x)
    def start_level = binarizedgradientpyramid_start_level(__x)
    def levels = binarizedgradientpyramid_levels(__x)
    def fixedLevelIndex = binarizedgradientpyramid_fixedLevelIndex(__x)
  }

  //object defs
  def binarizedgradientpyramid_obj_new(pyramid: Rep[Vector[GrayscaleImage]], start_level: Rep[Int], levels: Rep[Int], fixedLevelIndex: Rep[Int]): Rep[BinarizedGradientPyramid]

  //class defs
  def binarizedgradientpyramid_pyramid(__x: Rep[BinarizedGradientPyramid]): Rep[Vector[GrayscaleImage]]
  def binarizedgradientpyramid_start_level(__x: Rep[BinarizedGradientPyramid]): Rep[Int]
  def binarizedgradientpyramid_levels(__x: Rep[BinarizedGradientPyramid]): Rep[Int]
  def binarizedgradientpyramid_fixedLevelIndex(__x: Rep[BinarizedGradientPyramid]): Rep[Int]
}

trait BinarizedGradientPyramidOpsExp extends BinarizedGradientPyramidOps with EffectExp {
  case class BinarizedGradientPyramidObjectNew(pyramid: Exp[Vector[GrayscaleImage]], start_level: Exp[Int], levels: Exp[Int], fixedLevelIndex: Exp[Int]) extends Def[BinarizedGradientPyramid]
  case class BinarizedGradientPyramidPyramid(__x: Exp[BinarizedGradientPyramid]) extends Def[Vector[GrayscaleImage]]
  case class BinarizedGradientPyramidStart_level(__x: Exp[BinarizedGradientPyramid]) extends Def[Int]
  case class BinarizedGradientPyramidLevels(__x: Exp[BinarizedGradientPyramid]) extends Def[Int]
  case class BinarizedGradientPyramidFixedlevelindex(__x: Exp[BinarizedGradientPyramid]) extends Def[Int]

  def binarizedgradientpyramid_obj_new(pyramid: Exp[Vector[GrayscaleImage]], start_level: Exp[Int], levels: Exp[Int], fixedLevelIndex: Exp[Int]) = reflectEffect(BinarizedGradientPyramidObjectNew(pyramid, start_level, levels, fixedLevelIndex))
  def binarizedgradientpyramid_pyramid(__x: Rep[BinarizedGradientPyramid]) = BinarizedGradientPyramidPyramid(__x)
  def binarizedgradientpyramid_start_level(__x: Rep[BinarizedGradientPyramid]) = BinarizedGradientPyramidStart_level(__x)
  def binarizedgradientpyramid_levels(__x: Rep[BinarizedGradientPyramid]) = BinarizedGradientPyramidLevels(__x)
  def binarizedgradientpyramid_fixedLevelIndex(__x: Rep[BinarizedGradientPyramid]) = BinarizedGradientPyramidFixedlevelindex(__x)
}

trait ScalaGenBinarizedGradientPyramidOps extends ScalaGenBase {
  val IR: ApplicationOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  // these are the ops that call through to the underlying real data structure
    case BinarizedGradientPyramidObjectNew(pyramid, start_level, levels, fixedLevelIndex) => emitValDef(sym, "new " + remap(manifest[BinarizedGradientPyramid]) + "(" + quote(pyramid)  + "," + quote(start_level)  + "," + quote(levels)  + "," + quote(fixedLevelIndex)  + ")")
    case BinarizedGradientPyramidPyramid(x) =>  emitValDef(sym, quote(x) + ".pyramid")
    case BinarizedGradientPyramidStart_level(x) =>  emitValDef(sym, quote(x) + ".start_level")
    case BinarizedGradientPyramidLevels(x) =>  emitValDef(sym, quote(x) + ".levels")
    case BinarizedGradientPyramidFixedlevelindex(x) =>  emitValDef(sym, quote(x) + ".fixedLevelIndex")
    case _ => super.emitNode(sym, rhs)
  }
}


trait BinarizedGradientTemplateOps extends DSLType with Variables with OverloadHack {

  object BinarizedGradientTemplate {
    def apply(radius: Rep[Int], rect: Rep[Rect], mask_list: Rep[Vector[Int]], level: Rep[Int], binary_gradients: Rep[Vector[Int]], match_list: Rep[IndexVector], occlusions: Rep[Vector[Vector[Int]]], templates: Rep[Vector[BinarizedGradientTemplate]], hist: Rep[Vector[Float]]) = binarizedgradienttemplate_obj_new(radius, rect, mask_list, level, binary_gradients, match_list, occlusions, templates, hist)
  }

  implicit def repBinarizedGradientTemplateToBinarizedGradientTemplateOps(x: Rep[BinarizedGradientTemplate]) = new binarizedgradienttemplateOpsCls(x)
  implicit def binarizedgradienttemplateToBinarizedGradientTemplateOps(x: Var[BinarizedGradientTemplate]) = new binarizedgradienttemplateOpsCls(readVar(x))

  class binarizedgradienttemplateOpsCls(__x: Rep[BinarizedGradientTemplate]) {
    def radius = binarizedgradienttemplate_radius(__x)
    def rect = binarizedgradienttemplate_rect(__x)
    def mask_list = binarizedgradienttemplate_mask_list(__x)
    def level = binarizedgradienttemplate_level(__x)
    def binary_gradients = binarizedgradienttemplate_binary_gradients(__x)
    def match_list = binarizedgradienttemplate_match_list(__x)
    def occlusions = binarizedgradienttemplate_occlusions(__x)
    def templates = binarizedgradienttemplate_templates(__x)
    def hist = binarizedgradienttemplate_hist(__x)
  }

  //object defs
  def binarizedgradienttemplate_obj_new(radius: Rep[Int], rect: Rep[Rect], mask_list: Rep[Vector[Int]], level: Rep[Int], binary_gradients: Rep[Vector[Int]], match_list: Rep[IndexVector], occlusions: Rep[Vector[Vector[Int]]], templates: Rep[Vector[BinarizedGradientTemplate]], hist: Rep[Vector[Float]]): Rep[BinarizedGradientTemplate]

  //class defs
  def binarizedgradienttemplate_radius(__x: Rep[BinarizedGradientTemplate]): Rep[Int]
  def binarizedgradienttemplate_rect(__x: Rep[BinarizedGradientTemplate]): Rep[Rect]
  def binarizedgradienttemplate_mask_list(__x: Rep[BinarizedGradientTemplate]): Rep[Vector[Int]]
  def binarizedgradienttemplate_level(__x: Rep[BinarizedGradientTemplate]): Rep[Int]
  def binarizedgradienttemplate_binary_gradients(__x: Rep[BinarizedGradientTemplate]): Rep[Vector[Int]]
  def binarizedgradienttemplate_match_list(__x: Rep[BinarizedGradientTemplate]): Rep[IndexVector]
  def binarizedgradienttemplate_occlusions(__x: Rep[BinarizedGradientTemplate]): Rep[Vector[Vector[Int]]]
  def binarizedgradienttemplate_templates(__x: Rep[BinarizedGradientTemplate]): Rep[Vector[BinarizedGradientTemplate]]
  def binarizedgradienttemplate_hist(__x: Rep[BinarizedGradientTemplate]): Rep[Vector[Float]]
}

trait BinarizedGradientTemplateOpsExp extends BinarizedGradientTemplateOps with EffectExp {
  case class BinarizedGradientTemplateObjectNew(radius: Exp[Int], rect: Exp[Rect], mask_list: Exp[Vector[Int]], level: Exp[Int], binary_gradients: Exp[Vector[Int]], match_list: Exp[IndexVector], occlusions: Exp[Vector[Vector[Int]]], templates: Exp[Vector[BinarizedGradientTemplate]], hist: Exp[Vector[Float]]) extends Def[BinarizedGradientTemplate]
  case class BinarizedGradientTemplateRadius(__x: Exp[BinarizedGradientTemplate]) extends Def[Int]
  case class BinarizedGradientTemplateRect(__x: Exp[BinarizedGradientTemplate]) extends Def[Rect]
  case class BinarizedGradientTemplateMask_list(__x: Exp[BinarizedGradientTemplate]) extends Def[Vector[Int]]
  case class BinarizedGradientTemplateLevel(__x: Exp[BinarizedGradientTemplate]) extends Def[Int]
  case class BinarizedGradientTemplateBinary_gradients(__x: Exp[BinarizedGradientTemplate]) extends Def[Vector[Int]]
  case class BinarizedGradientTemplateMatch_list(__x: Exp[BinarizedGradientTemplate]) extends Def[IndexVector]
  case class BinarizedGradientTemplateOcclusions(__x: Exp[BinarizedGradientTemplate]) extends Def[Vector[Vector[Int]]]
  case class BinarizedGradientTemplateTemplates(__x: Exp[BinarizedGradientTemplate]) extends Def[Vector[BinarizedGradientTemplate]]
  case class BinarizedGradientTemplateHist(__x: Exp[BinarizedGradientTemplate]) extends Def[Vector[Float]]

  def binarizedgradienttemplate_obj_new(radius: Exp[Int], rect: Exp[Rect], mask_list: Exp[Vector[Int]], level: Exp[Int], binary_gradients: Exp[Vector[Int]], match_list: Exp[IndexVector], occlusions: Exp[Vector[Vector[Int]]], templates: Exp[Vector[BinarizedGradientTemplate]], hist: Exp[Vector[Float]]) = reflectEffect(BinarizedGradientTemplateObjectNew(radius, rect, mask_list, level, binary_gradients, match_list, occlusions, templates, hist))
  def binarizedgradienttemplate_radius(__x: Rep[BinarizedGradientTemplate]) = BinarizedGradientTemplateRadius(__x)
  def binarizedgradienttemplate_rect(__x: Rep[BinarizedGradientTemplate]) = BinarizedGradientTemplateRect(__x)
  def binarizedgradienttemplate_mask_list(__x: Rep[BinarizedGradientTemplate]) = BinarizedGradientTemplateMask_list(__x)
  def binarizedgradienttemplate_level(__x: Rep[BinarizedGradientTemplate]) = BinarizedGradientTemplateLevel(__x)
  def binarizedgradienttemplate_binary_gradients(__x: Rep[BinarizedGradientTemplate]) = BinarizedGradientTemplateBinary_gradients(__x)
  def binarizedgradienttemplate_match_list(__x: Rep[BinarizedGradientTemplate]) = BinarizedGradientTemplateMatch_list(__x)
  def binarizedgradienttemplate_occlusions(__x: Rep[BinarizedGradientTemplate]) = BinarizedGradientTemplateOcclusions(__x)
  def binarizedgradienttemplate_templates(__x: Rep[BinarizedGradientTemplate]) = BinarizedGradientTemplateTemplates(__x)
  def binarizedgradienttemplate_hist(__x: Rep[BinarizedGradientTemplate]) = BinarizedGradientTemplateHist(__x)
}

trait ScalaGenBinarizedGradientTemplateOps extends ScalaGenBase {
  val IR: ApplicationOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  // these are the ops that call through to the underlying real data structure
    case BinarizedGradientTemplateObjectNew(radius, rect, mask_list, level, binary_gradients, match_list, occlusions, templates, hist) => emitValDef(sym, "new " + remap(manifest[BinarizedGradientTemplate]) + "(" + quote(radius)  + "," + quote(rect)  + "," + quote(mask_list)  + "," + quote(level)  + "," + quote(binary_gradients)  + "," + quote(match_list)  + "," + quote(occlusions)  + "," + quote(templates)  + "," + quote(hist)  + ")")
    case BinarizedGradientTemplateRadius(x) =>  emitValDef(sym, quote(x) + ".radius")
    case BinarizedGradientTemplateRect(x) =>  emitValDef(sym, quote(x) + ".rect")
    case BinarizedGradientTemplateMask_list(x) =>  emitValDef(sym, quote(x) + ".mask_list")
    case BinarizedGradientTemplateLevel(x) =>  emitValDef(sym, quote(x) + ".level")
    case BinarizedGradientTemplateBinary_gradients(x) =>  emitValDef(sym, quote(x) + ".binary_gradients")
    case BinarizedGradientTemplateMatch_list(x) =>  emitValDef(sym, quote(x) + ".match_list")
    case BinarizedGradientTemplateOcclusions(x) =>  emitValDef(sym, quote(x) + ".occlusions")
    case BinarizedGradientTemplateTemplates(x) =>  emitValDef(sym, quote(x) + ".templates")
    case BinarizedGradientTemplateHist(x) =>  emitValDef(sym, quote(x) + ".hist")
    case _ => super.emitNode(sym, rhs)
  }
}


trait RectOps extends DSLType with Variables with OverloadHack {

  object Rect {
    def apply(x: Rep[Int], y: Rep[Int], width: Rep[Int], height: Rep[Int]) = rect_obj_new(x, y, width, height)
  }

  implicit def repRectToRectOps(x: Rep[Rect]) = new rectOpsCls(x)
  implicit def rectToRectOps(x: Var[Rect]) = new rectOpsCls(readVar(x))

  class rectOpsCls(__x: Rep[Rect]) {
    def x = rect_x(__x)
    def y = rect_y(__x)
    def width = rect_width(__x)
    def height = rect_height(__x)
  }

  //object defs
  def rect_obj_new(x: Rep[Int], y: Rep[Int], width: Rep[Int], height: Rep[Int]): Rep[Rect]

  //class defs
  def rect_x(__x: Rep[Rect]): Rep[Int]
  def rect_y(__x: Rep[Rect]): Rep[Int]
  def rect_width(__x: Rep[Rect]): Rep[Int]
  def rect_height(__x: Rep[Rect]): Rep[Int]
}

trait RectOpsExp extends RectOps with EffectExp {
  case class RectObjectNew(x: Exp[Int], y: Exp[Int], width: Exp[Int], height: Exp[Int]) extends Def[Rect]
  case class RectX(__x: Exp[Rect]) extends Def[Int]
  case class RectY(__x: Exp[Rect]) extends Def[Int]
  case class RectWidth(__x: Exp[Rect]) extends Def[Int]
  case class RectHeight(__x: Exp[Rect]) extends Def[Int]

  def rect_obj_new(x: Exp[Int], y: Exp[Int], width: Exp[Int], height: Exp[Int]) = reflectEffect(RectObjectNew(x, y, width, height))
  def rect_x(__x: Rep[Rect]) = RectX(__x)
  def rect_y(__x: Rep[Rect]) = RectY(__x)
  def rect_width(__x: Rep[Rect]) = RectWidth(__x)
  def rect_height(__x: Rep[Rect]) = RectHeight(__x)
}

trait ScalaGenRectOps extends ScalaGenBase {
  val IR: ApplicationOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  // these are the ops that call through to the underlying real data structure
    case RectObjectNew(x, y, width, height) => emitValDef(sym, "new " + remap(manifest[Rect]) + "(" + quote(x)  + "," + quote(y)  + "," + quote(width)  + "," + quote(height)  + ")")
    case RectX(x) =>  emitValDef(sym, quote(x) + ".x")
    case RectY(x) =>  emitValDef(sym, quote(x) + ".y")
    case RectWidth(x) =>  emitValDef(sym, quote(x) + ".width")
    case RectHeight(x) =>  emitValDef(sym, quote(x) + ".height")
    case _ => super.emitNode(sym, rhs)
  }
}

trait APairOps extends DSLType with Variables with OverloadHack {

  object APair {
    def apply(_1: Rep[ACluster], _2: Rep[Double]) = apair_obj_new(_1, _2)
  }

  implicit def repAPairToAPairOps(x: Rep[APair]) = new apairOpsCls(x)
  implicit def apairToAPairOps(x: Var[APair]) = new apairOpsCls(readVar(x))

  class apairOpsCls(__x: Rep[APair]) {
    def _1 = apair__1(__x)
    def _2 = apair__2(__x)
  }

  //object defs
  def apair_obj_new(_1: Rep[ACluster], _2: Rep[Double]): Rep[APair]

  //class defs
  def apair__1(__x: Rep[APair]): Rep[ACluster]
  def apair__2(__x: Rep[APair]): Rep[Double]
}

trait APairOpsExp extends APairOps with EffectExp {
  case class APairObjectNew(_1: Exp[ACluster], _2: Exp[Double]) extends Def[APair]
  case class APair_1(__x: Exp[APair]) extends Def[ACluster]
  case class APair_2(__x: Exp[APair]) extends Def[Double]

  def apair_obj_new(_1: Exp[ACluster], _2: Exp[Double]) = reflectEffect(APairObjectNew(_1, _2))
  def apair__1(__x: Rep[APair]) = APair_1(__x)
  def apair__2(__x: Rep[APair]) = APair_2(__x)
}

trait ScalaGenAPairOps extends ScalaGenBase {
  val IR: ApplicationOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  // these are the ops that call through to the underlying real data structure
    case APairObjectNew(_1, _2) => emitValDef(sym, "new " + remap(manifest[APair]) + "(" + quote(_1)  + "," + quote(_2)  + ")")
    case APair_1(x) =>  emitValDef(sym, quote(x) + "._1")
    case APair_2(x) =>  emitValDef(sym, quote(x) + "._2")
    case _ => super.emitNode(sym, rhs)
  }
}

trait PQOps extends DSLType with Variables with OverloadHack {

  object PQ {
    def apply(fold: Rep[Int]) = pq_obj_new(fold)
  }

  implicit def repPQToPQOps(x: Rep[PQ]) = new pqOpsCls(x)
  implicit def pqToPQOps(x: Var[PQ]) = new pqOpsCls(readVar(x))

  class pqOpsCls(__x: Rep[PQ]) {
    def fold = pq_fold(__x)
    def Q = pq_Q(__x)
    def MMD = pq_MMD(__x)
    def empty = pq_empty(__x)
    def normalize = pq_normalize(__x)
    def push(__p: Rep[APair]) = pq_push(__x, __p)
    def push(__c: Rep[ACluster], __d: Rep[Double]) = pq_push(__x, __c, __d)
    def merge(__pq: Rep[PQ]) = pq_merge(__x, __pq)
    def top = pq_top(__x)
    def pop = pq_pop(__x)
  }

  //object defs
  def pq_obj_new(fold: Rep[Int]): Rep[PQ]

  //class defs
  def pq_fold(__x: Rep[PQ]): Rep[Int]
  def pq_Q(__x: Rep[PQ]): Rep[java.util.PriorityQueue[APair]]
  def pq_MMD(__x: Rep[PQ]): Rep[Double]
  def pq_empty(__x: Rep[PQ]): Rep[Boolean]
  def pq_normalize(__x: Rep[PQ]): Rep[Unit]
  def pq_push(__x: Rep[PQ], __p: Rep[APair]): Rep[Unit]
  def pq_push(__x: Rep[PQ], __c: Rep[ACluster], __d: Rep[Double]): Rep[Unit]
  def pq_merge(__x: Rep[PQ], __pq: Rep[PQ]): Rep[Unit]
  def pq_top(__x: Rep[PQ]): Rep[ACluster]
  def pq_pop(__x: Rep[PQ]): Rep[Unit]
}

trait PQOpsExp extends PQOps with EffectExp {
  case class PQObjectNew(fold: Exp[Int]) extends Def[PQ]
  case class PQFold(__x: Exp[PQ]) extends Def[Int]
  case class PQQ(__x: Exp[PQ]) extends Def[java.util.PriorityQueue[APair]]
  case class PQMMD(__x: Exp[PQ]) extends Def[Double]
  case class PQEmpty(__x: Exp[PQ]) extends Def[Boolean]
  case class PQNormalize(__x: Exp[PQ]) extends Def[Unit]
  case class PQPush1(__x: Exp[PQ], __p: Exp[APair]) extends Def[Unit]
  case class PQPush2(__x: Exp[PQ], __c: Exp[ACluster], __d: Exp[Double]) extends Def[Unit]
  case class PQMerge(__x: Exp[PQ], __pq: Exp[PQ]) extends Def[Unit]
  case class PQTop(__x: Exp[PQ]) extends Def[ACluster]
  case class PQPop(__x: Exp[PQ]) extends Def[Unit]

  def pq_obj_new(fold: Exp[Int]) = reflectMutable(PQObjectNew(fold))
  def pq_fold(__x: Exp[PQ]) = PQFold(__x)
  def pq_Q(__x: Exp[PQ]) = PQQ(__x)
  def pq_MMD(__x: Exp[PQ]) = PQMMD(__x)
  def pq_empty(__x: Exp[PQ]) = PQEmpty(__x)
  def pq_normalize(__x: Exp[PQ]) = reflectWrite(__x)(PQNormalize(__x))
  def pq_push(__x: Exp[PQ], __p: Exp[APair]) = reflectWrite(__x)(PQPush1(__x, __p))
  def pq_push(__x: Exp[PQ], __c: Exp[ACluster], __d: Exp[Double]) = reflectWrite(__x)(PQPush2(__x, __c, __d))
  def pq_merge(__x: Exp[PQ], __pq: Exp[PQ]) = reflectWrite(__x)(PQMerge(__x, __pq))
  def pq_top(__x: Exp[PQ]) = PQTop(__x)
  def pq_pop(__x: Exp[PQ]) = reflectWrite(__x)(PQPop(__x))
}

trait ScalaGenPQOps extends ScalaGenBase {
  val IR: ApplicationOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  // these are the ops that call through to the underlying real data structure
    case PQObjectNew(fold) => emitValDef(sym, "new " + remap(manifest[PQ]) + "(" + quote(fold)  + ")")
    case PQFold(x) =>  emitValDef(sym, quote(x) + ".fold")
    case PQQ(x) => emitVarDef(sym, "new java.util.PriorityQueue[APair]")
    case PQMMD(x) => emitVarDef(sym, "scala.Double.MaxValue")
    case PQEmpty(x) => emitValDef(sym, quote(x) + ".empty")
    case PQNormalize(x) => emitValDef(sym, quote(x) + ".normalize")
    case PQPush1(x,p) => emitValDef(sym, quote(x) + ".push(" + quote(p) + ")")
    case PQPush2(x,c,d) => emitValDef(sym, quote(x) + ".push(" + quote(c) + "," + quote(d) + ")")
    case PQMerge(x,pq) => emitValDef(sym, quote(x) + ".merge(" + quote(pq) + ")")
    case PQTop(x) => emitValDef(sym, quote(x) + ".top()")
    case PQPop(x) => emitValDef(sym, quote(x) + ".pop()")
    case _ => super.emitNode(sym, rhs)
  }
}

trait AClusterOps extends DSLType with Variables with OverloadHack {

  object ACluster {
    def apply(dim: Rep[Int]) = acluster_obj_new(dim)
  }

  implicit def repAClusterToAClusterOps(x: Rep[ACluster]) = new aclusterOpsCls(x)
  implicit def aclusterToAClusterOps(x: Var[ACluster]) = new aclusterOpsCls(readVar(x))

  class aclusterOpsCls(__x: Rep[ACluster]) {
    def dim = acluster_dim(__x)
    // def centers
    def index = acluster_index(__x)
    def offset = acluster_offset(__x)
    def valid = acluster_valid(__x)
    def valid_=(v:Rep[Boolean]) = acluster_valid_set(__x,v)
    def merged = acluster_merged(__x)
    def merged_=(m:Rep[Boolean]) = acluster_merged_set(__x,m)
    def members = acluster_members(__x)
    def num_members = acluster_num_members(__x)
    // def data
    def init_RM(d:Rep[Vector[Double]], c:Rep[Vector[Double]], m:Rep[Vector[Int]], i:Rep[Int]) = acluster_init_RM(__x,d,c,m,i)
    def reset_RM = acluster_reset_RM(__x)
    def push_on_pq(from: Rep[ACluster], pq: Rep[PQ]) = acluster_push_on_pq(__x,from,pq)
    def getCandidates(from: Rep[ACluster]) = acluster_getCandidates(__x,from)
    def mergeCandidates(c: Rep[Vector[APair]], l: Rep[Int]) = acluster_mergeCandidates(__x, c, l)
    def merge_in_pq(pq: Rep[PQ]) = acluster_merge_in_pq(__x,pq)
  }

  //object defs
  def acluster_obj_new(dim: Rep[Int]): Rep[ACluster]

  //class defs
  def acluster_dim(__x: Rep[ACluster]): Rep[Int]
  def acluster_index(__x: Rep[ACluster]): Rep[Int]
  def acluster_offset(__x: Rep[ACluster]): Rep[Int]
  def acluster_valid(__x: Rep[ACluster]): Rep[Boolean]
  def acluster_valid_set(__x: Rep[ACluster], v: Rep[Boolean]): Rep[Unit]
  def acluster_merged(__x: Rep[ACluster]): Rep[Boolean]
  def acluster_merged_set(__x: Rep[ACluster], m: Rep[Boolean]): Rep[Unit]
  def acluster_members(__x: Rep[ACluster]): Rep[Vector[Int]]
  def acluster_num_members(__x: Rep[ACluster]): Rep[Int]
  def acluster_init_RM(__x:Rep[ACluster], d:Rep[Vector[Double]], c:Rep[Vector[Double]], m:Rep[Vector[Int]], i:Rep[Int]): Rep[Unit]
  def acluster_reset_RM(__x: Rep[ACluster]): Rep[Unit]
  def acluster_push_on_pq(__x: Rep[ACluster], from: Rep[ACluster], pq:Rep[PQ]): Rep[Unit]
  def acluster_merge_in_pq(__x: Rep[ACluster], pq: Rep[PQ]): Rep[Unit]
  def acluster_getCandidates(__x: Rep[ACluster], from: Rep[ACluster]): Rep[APair]
  def acluster_mergeCandidates(__x: Rep[ACluster], candidates:Rep[Vector[APair]], fold:Rep[Int]): Rep[Unit]
}

trait AClusterOpsExp extends AClusterOps with EffectExp {

  this: OptiMLExp =>

  case class AClusterObjectNew(dim: Exp[Int]) extends Def[ACluster]
  case class AClusterDim(__x: Exp[ACluster]) extends Def[Int]
  case class AClusterIndex(__x: Exp[ACluster]) extends Def[Int]
  case class AClusterOffset(__x: Exp[ACluster]) extends Def[Int]
  case class AClusterValid(__x: Exp[ACluster]) extends Def[Boolean]
  case class AClusterValidSet(__x: Exp[ACluster], v: Exp[Boolean]) extends Def[Unit]
  case class AClusterMergedSet(__x: Exp[ACluster], m: Exp[Boolean]) extends Def[Unit]
  case class AClusterMerged(__x: Exp[ACluster]) extends Def[Boolean]
  case class AClusterMembers(__x: Exp[ACluster]) extends Def[Vector[Int]]
  case class AClusterNumMembers(__x: Exp[ACluster]) extends Def[Int]
  case class AClusterInitRM(x:Exp[ACluster], d:Exp[Vector[Double]], c:Exp[Vector[Double]], m:Exp[Vector[Int]], i:Exp[Int]) extends Def[Unit]
  case class AClusterResetRM(__x: Exp[ACluster]) extends Def[Unit]
  case class AClusterPushOnPQ(__x:Exp[ACluster], from:Exp[ACluster], pq:Exp[PQ]) extends Def[Unit]
  case class AClusterGetCandidates(__x: Exp[ACluster], from: Exp[ACluster]) extends Def[APair]
  case class AClusterMergeCandidates(__x: Exp[ACluster], candidates:Exp[Vector[APair]], fold:Exp[Int]) extends Def[Unit]
  case class AClusterMergeInPQ(__x: Exp[ACluster], pq: Exp[PQ]) extends Def[Unit]
  
  def acluster_obj_new(dim: Exp[Int]) = reflectMutable(AClusterObjectNew(dim))
  def acluster_dim(__x: Exp[ACluster]) = AClusterDim(__x)
  def acluster_index(__x: Exp[ACluster]): Exp[Int] = AClusterIndex(__x)
  def acluster_offset(__x: Exp[ACluster]): Exp[Int] = AClusterOffset(__x)
  def acluster_valid(__x: Exp[ACluster]): Exp[Boolean] = AClusterValid(__x)
  def acluster_valid_set(__x: Exp[ACluster], valid: Exp[Boolean]) = __x match {
    case Def(Reflect(VectorApply(v,n),_,_)) => reflectWrite(v)(AClusterValidSet(__x, valid))
    case _ => reflectWrite(__x)(AClusterValidSet(__x, valid))
  }
  def acluster_merged(__x: Exp[ACluster]): Exp[Boolean] = AClusterMerged(__x)
  def acluster_merged_set(__x: Exp[ACluster], m: Exp[Boolean]) = __x match{
    case Def(Reflect(VectorApply(v,n),_,_)) => reflectWrite(v)(AClusterMergedSet(__x, m))
    case _ => reflectWrite(__x)(AClusterMergedSet(__x, m))
  }
  def acluster_members(__x: Exp[ACluster]): Exp[Vector[Int]] = AClusterMembers(__x)
  def acluster_num_members(__x: Exp[ACluster]): Exp[Int] = AClusterNumMembers(__x)
  def acluster_init_RM(__x:Exp[ACluster], d:Exp[Vector[Double]], c:Exp[Vector[Double]], m:Exp[Vector[Int]], i:Exp[Int]) = __x match{
    case Def(Reflect(VectorApply(v,n),_,_)) => reflectWrite(v)(AClusterInitRM(__x,d,c,m,i))
    case _ => reflectWrite(__x)(AClusterInitRM(__x,d,c,m,i))
  }
  def acluster_reset_RM(__x: Exp[ACluster]): Exp[Unit] = __x match{
    case Def(Reflect(VectorApply(v,n),_,_)) => reflectWrite(v)(AClusterResetRM(__x))
    case _ => reflectWrite(__x)(AClusterResetRM(__x))
  }
  def acluster_push_on_pq(__x: Exp[ACluster], from: Exp[ACluster], pq: Exp[PQ]) = __x match{
    case Def(Reflect(VectorApply(v,n),_,_)) => reflectWrite(v)(AClusterPushOnPQ(__x,from,pq))
    case _ => reflectWrite(__x)(AClusterPushOnPQ(__x,from,pq))
  }
  def acluster_getCandidates(__x: Exp[ACluster], from: Exp[ACluster]) = AClusterGetCandidates(__x, from)
  def acluster_mergeCandidates(__x: Exp[ACluster], candidates:Exp[Vector[APair]], fold:Exp[Int]) = __x match{
    case Def(Reflect(VectorApply(v,n),_,_)) => reflectWrite(v)(AClusterMergeCandidates(__x, candidates, fold))
    case _ => reflectWrite(__x)(AClusterMergeCandidates(__x, candidates, fold))
  }
  def acluster_merge_in_pq(__x: Exp[ACluster], pq:Exp[PQ]) = __x match{
    case Def(Reflect(VectorApply(v,n),_,_)) => reflectWrite(v)(AClusterMergeInPQ(__x,pq))
    case _ => reflectWrite(__x)(AClusterMergeInPQ(__x,pq))
  }
}

trait ScalaGenAClusterOps extends ScalaGenBase {
  val IR: ApplicationOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  // these are the ops that call through to the underlying real data structure
    case AClusterObjectNew(dim) => emitValDef(sym, "new " + remap(manifest[ACluster]) + "(" + quote(dim)  + ")")
    case AClusterDim(x) =>  emitValDef(sym, quote(x) + ".dim")
    case AClusterIndex(x) =>  emitValDef(sym, quote(x) + ".index")
    case AClusterOffset(x) =>  emitValDef(sym, quote(x) + ".offset")
    case AClusterValid(x) =>  emitValDef(sym, quote(x) + ".valid")
    case AClusterValidSet(x,v) => emitValDef(sym, quote(x) + ".valid = " + quote(v))
    case AClusterMerged(x) =>  emitValDef(sym, quote(x) + ".merged")
    case AClusterMergedSet(x,v) => emitValDef(sym, quote(x) + ".merged = " + quote(v))
    case AClusterMembers(x) =>  emitValDef(sym, quote(x) + ".members")
    case AClusterNumMembers(x) =>  emitValDef(sym, quote(x) + ".num_members")
    case AClusterInitRM(x,d,c,m,i) =>  emitValDef(sym, quote(x) + ".init_RM(" + quote(d) + "," + quote(c) + "," + quote(m) + "," + quote(i) + ")")
    case AClusterResetRM(x) => emitValDef(sym, quote(x) + ".reset_RM")
    case AClusterPushOnPQ(x,from,pq) => emitValDef(sym, quote(x) + ".push_on_pq(" + quote(from) + ", " + quote(pq) + ")")
    case AClusterGetCandidates(x,f) => emitValDef(sym, quote(x) + ".getCandidates(" + quote(f) + ")")
    case AClusterMergeCandidates(x,c,f) => emitValDef(sym, quote(x) + ".mergeCandidates(" + quote(c) + "," + quote(f) + ")")
    case AClusterMergeInPQ(x,pq) => emitValDef(sym, quote(x) + ".merge_in_pq(" + quote(pq) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}


