{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Shapes as Sh
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R
import Text.Blaze.Svg11
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A2
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Text.Blaze.Html.Renderer.Text
import Data.Text.Lazy
import Text.Blaze.Internal (MarkupM (Append))

--Scotty web handling
main = scotty 3000 $ do
  get "/" $ do
    html $ homepage

--Handles Sahpe conversion request
  get "/shapes/:drawing" $ do
    paramDrawing <- param "drawing"
    let drawingObjs = read (unpack paramDrawing) :: Drawing
    let svg = drawingConverter drawingObjs
    let dString =renderSvg svg
    html $ shaperesult paramDrawing dString svg

--HTML gnerator for homepage
homepage :: Text
homepage = do
  R.renderHtml $ do
    H.head $ H.title "Shapes"
    H.body $ do
      H.h1 "Shape Homepage"
      H.p "New Transforms added that set the style of a SVG item: "
      H.ul $ do
        H.li "Shading Colour  => Changes colour of shape (Default Black)"
        H.li "Stroke Colour  => Sets colour of border (Default None)"
        H.li "StrokeWidth Int => Sets width"
      H.div $ do
        H.p "Please enter your Drawing to convert to SVG as a GET request to ../shapes/"
        H.p "Expects Drawing in the form: [(Transform, Shape)]"
        H.p "Example query: localhost:3000/shapes/[(Compose (Translate (Vector 300.0 3.0))(Compose (StrokeWidth 3) (Stroke Red)),Circle), (Compose (Compose (Translate (Vector 0.0 250.0))(Compose (Scale (Vector 2.0 2.0)) (Shading Green))) (Stroke Black) ,Circle), (Compose (Rotate 50 40 160) (Compose (Shading White) (Stroke Cyan)),Square)]"


--HTMl generator for result of shape query
shaperesult :: Text -> [Char] -> S.Svg -> Text
shaperesult shapeInput svgIn svgOut = do
  R.renderHtml $ do
    H.head $ H.title "Shapes Result"
    H.body $ do
      H.h1 "Shapes Result"
      H.p ("Shape Input:  " >> H.toHtml shapeInput)
      H.p ("Output SVG in HTML:  " >> H.toHtml svgIn)
      H.p "SVG image:"
      svgOut

--Wrapper class for drawing conversionS
--Combines SVG template with result of input drawing
drawingConverter :: [(Sh.Transform, Sh.Shape)] -> S.Svg
drawingConverter drawing = basicSvg $ do
  drawingConverter2 drawing

--Takes Drawing type as input and produces resulting SVG
-- SVG combination of result of the input Drawing
drawingConverter2 :: [(Sh.Transform, Sh.Shape)] -> S.Svg
drawingConverter2 ((transform, shape):[]) =
    shapeConverter (transform, shape)
drawingConverter2 ((transform, shape):rest) = Append (shapeConverter (transform, shape)) (drawingConverter2 rest)

-- Takes a single Shape item (Transform,Shape) and returns resulting SVG
-- In general form: shapeTemplate ! Transform ! Style
shapeConverter :: (Sh.Transform, Sh.Shape) -> S.Svg
shapeConverter (transform, Square) = basicSquare ! A2.transform (transformAttributes transform) ! A2.style (styleAttributes transform)
shapeConverter (transform, Circle) = basicCircle ! A2.transform (transformAttributes transform) ! A2.style (styleAttributes transform)
shapeConverter (transform, Empty) =  S.image  --base case - just returns SVG which doesn't represent anything

-- Wrapper funcion for transform attributes
-- Converts Transform given into a single SVG attribute value
transformAttributes :: Sh.Transform -> S.AttributeValue
transformAttributes t = S.stringValue (transformString t)

--Converts Transform given into a string representing attribute value
-- String used so can combine the result of 2+ transforms => Handle Compose
transformString :: Sh.Transform -> [Char]
transformString (Compose t0 t1) = transformString t0 ++ transformString t1
--transformString (Rotate (Sh.Matrix (Sh.Vector x y) (Sh.Vector x2 y2))) = "rotate(" ++ show x ++ " " ++ show y ++ " " ++ show x2 ++ " " ++ show y2 ++ ")"
transformString (Rotate x y angle) = "rotate("  ++ show angle ++ " " ++ show x ++ " " ++ show y ++ ")"
transformString (Identity) = ""
transformString (Scale (Sh.Vector x y)) = "scale(" ++ show x ++ "," ++ show y ++")"
transformString (Translate (Sh.Vector x y)) = "translate(" ++ show x ++ "," ++ show y ++")"
transformString (_) = ""

-- Wrapper funcion for style attributes
-- Converts Transform given into a single SVG attribute value
styleAttributes :: Sh.Transform -> S.AttributeValue
styleAttributes t = S.stringValue (styleString t)

--Converts Transform given into a string representing attribute value for style
-- String used so can combine the result of 2+ transforms => Handle Compose
styleString :: Sh.Transform -> [Char]
styleString (Compose t0 t1) = styleString t0 ++ styleString t1
styleString (Shading c) = "fill:" ++ show c ++ ";"
styleString (Stroke c) = "stroke:" ++ show c ++ ";"
styleString (StrokeWidth w) = "stroke-width:" ++ show w ++ ";"
styleString (_) = ""

--Standard SVG Attributes, used as templates
basicSvg = S.svg ! A2.version "1.1" ! A2.width "500" ! A2.height "500" ! A2.style "border: 1px solid black;"
basicCircle =S.circle ! A2.cx "50" ! A2.cy "50" ! A2.r "40"
basicSquare = S.rect ! A2.width "40" ! A2.height "40"
