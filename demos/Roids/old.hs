

-- This one wraps.  But it looks terrible, because the Sprite library does
-- linear interpolation where we want a discontinuity.

flyShip3 u =
 move (viewWrap pos u) (flipImage shipBook page)
  `untilB` restart u ==> flyShip3
 where
   page  = shipPage angle
   pos   = integral vel u
   vel   = integral acc u
   angle = integral angularVel u
   acc	 = vector2Polar accelMag angle
   angularVel = rotateControl u
   accelMag   = thrustControl u


{-
-- This one bounces rather than wrapping.  Well -- it's *supposed to*, but
-- it just sits there GC'ing. :-(

flyShipBombs4 u =
 move pos (flipImage shipBook page)
  `untilB` restart u ==> flyShipBombs4
 where
   page  = shipPage angle
   pos   = integral vel u
   vel   = integral acc u + impulseTot
   angle = integral angularVel u
   acc	 = vector2Polar accelMag angle
   angularVel = rotateControl u
   accelMag   = thrustControl u

   -- Bounce event giving function to map impact velocity to impulse vector
   bounceFV :: Event (S.Vector2 -> S.Vector2)
   bounceFV =
         predicate vertHit  u -=> flipX
     .|. predicate horizHit u -=> flipY
     where
       vertHit  = x >=* xMax &&* vx >* 0 ||* x <=* xMin &&* vx <* 0 
       horizHit = y >=* yMax &&* vy >* 0 ||* y <=* yMin &&* vy <* 0 
       flipX (S.Vector2XY dx dy) = S.Vector2XY (-2 * dx) dy
       flipY (S.Vector2XY dx dy) = S.Vector2XY dx (-2 * dy)
       (x ,y ) = pairBSplit (vector2XYCoords pos)
       (vx,vy) = pairBSplit (vector2XYCoords vel)

       -- Box bounds.  Impulse should really take into account
       -- (derivative xMax) - x, etc.
       xMax = width/2 ; xMin = - xMax
       yMax = height/2; yMin = - yMax
       (width, height) = pairBSplit (vector2XYCoords (viewSize u))

   -- Bounce event with new impulse
   bounceImpulse =
     bounceFV `snapshot` vel ==> \ (fV, v1) -> fV v1

   impulseTot = loop S.zeroVector bounceImpulse
    where
     loop soFar impulse =
       lift0 soFar `untilB` impulse #+=> \ t imp impulse' ->
       loop (soFar + imp) impulse'
-}


-- Like flyShip1, but shoots
{-

flyShip5 u =
 missiles u `over` move pos (flipImage shipBook page)
  `untilB` restart u ==> flyShip5
 where
   page  = shipPage angle
   pos   = integral vel u
   vel   = vector2Polar speed angle
   angle = integral angularVel u
   speed = integral forwardAccel u
   angularVel   = rotateControl u
   forwardAccel = thrustControl u

   -- Generate a missile on each fire event
   {- This version makes fire get redone, and so hangs onto old pos,vel,
      which is very inefficient.   
   missiles u = emptyImage `untilB` fire u ==> \ (missile, u') ->
                missiles u' `over` missile
   -}
   missiles u = accumB over emptyImage (fire u)

   -- Fire at each key press
   fire u = fire u `snapshot` pairB pos angle ==>
            \ (u',(pos0, angle0)) ->
            -- trace ("fire " ++ show (userStartTime u') ++ "\n") $
            newMissile pos0 angle0 u'

   -- A pyramid shot at a constant velocity, lasting a while.
   newMissile p0 angle0 u =
     -- trace ("newMissile " ++ show (userStartTime u,p0,angle0) ++ "\n") $
     move mpos (flipImage pyramidBook (30 * time)
                `over` soundImage engineSound)
     `untilB` userTimeIs u (2 / missileSpeed) -=> emptyImage
    where
     mpos = lift0 p0 + (userTime u) *^ mvel
     mvel = vector2Polar (lift0 missileSpeed) (lift0 angle0)
     missileSpeed = 0.8
-}


viewWrap :: Vector2B -> User -> Vector2B

viewWrap pos u =
 vector2XY (iWrapB (-maxX) maxX posX) (iWrapB (-maxY) maxY posY)
 where
  (width, height) = vector2XYCoords (viewSize u)
  maxX = width/2 ; maxY = height/2
  (posX, posY) = vector2XYCoords pos


iWrapB :: RealB -> RealB -> RealB -> RealB

iWrapB = lift3 iWrap

iWrap :: RealFrac a => a -> a -> a -> a

iWrap lo hi x = x - wid * wraps
 where
  wid = hi - lo
  wraps = fromInt (floor ((x - lo) / wid))

linear :: S.Vector2 -> S.Vector2 -> Time -> Vector2B
linear p v t0 =
  --  Most declarative (but needs user instad of t0):
  -- constantB p + atRate (constantB v) t0
  --  Explicitly linear:
  -- constantB p + (time - constantB t0) * (constantB v)
  --  Most efficient:
  lift1 (\t -> S.Vector2XY (x0+dx*t) (y0+dy*t)) time
 where
   S.Vector2XY x0 y0 = p
   S.Vector2XY dx dy = v

{-
shoot :: RealB -> User -> Shooter
shoot angle u =
 where
   missileE =
     keyPress fireKey u `snapshot` angle
                        ==>        shootMissile
   shootMissile angleShoot motShoot velShoot tShoot env =
     colliderGen (motShoot, velShoot', zeroVector, Missile radius, neverE) env
        `untilB` timeIs (tShoot + dur) -=> NothingSS
    where
      radius    = 1 - (time - constantB tShoot) / dur  -- 1 to 0 in "dur" seconds
      velShoot' = velShoot + S.vector2Polar boost a
      boost     = 1  -- boost beyond ship velocity
      dur       = 2  -- missile's duration
-}
