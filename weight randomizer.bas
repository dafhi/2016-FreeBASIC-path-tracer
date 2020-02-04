#include "pathtracer x.bas"


#Define def  #define

#Undef Int
def Int  As Integer
def sng  As single


/' -- weight randomizer ---

 ! ! ! W A R N I N G ! ! !  -  lots of flashing here!

this demo renders spheres and one small, super-bright light ..
a difficult path tracer scene

test loop
  randomize weights
  render
  score
  if better, save
  else restore

weights are saved to "weights.txt"

 -----------

Making heavy changes to my path tracer.  When I envision a new variable for experimentation,
things happen in several locations:
 
 1.  "pathtracer .. .bas"

A. 3 places, starting at line 362 (all bunched up in one section)
B. how the variables relate w/ path tracer algorithm (can apply to any project)
 
 2.  weight randomizer (this file)

A. printout()
B. index_from_probability()
C. new_weights()
  1. change ubound time_spent_randomizing()
  2. adjust select case
D. main() - 2 sections

'/

type tEVS
  as eval_vars ptr  p
  as double         s
  as string         kstr
End Type

function round(in as double, places as ubyte = 0) as string
  dim as integer mul = 10 ^ places
  return str( flr( in * mul +.5 ) / mul )
End Function

sub printout(temp as tEVS, best as tEVS)
  locate 33,1
  
  ? round(temp.s), round(best.s)
  ?
  
  #define pr(parm) *temp.p. parm, *best.p. parm, "   "; #parm
  ? pr(parm0)
  ? pr(parm1)
  ? pr(parm2)
  ? pr(parm3)
  ? pr(parm4)
  ? pr(parm5)
  ? pr(parm6)
  ? pr(parm7)
  ? pr(parm8)
  ? pr(parm9)
  ? pr(parm10)
  ? pr(parm11)
  ? pr(parm12)
  ? pr(parm13)
  ? pr(parm14)
  ?
  ?
  ? "Allow randomization for at least a few seconds."
  ? "Press any key to render using new weights."
 
end sub

function index_from_probability(a() as single, not_value as integer = -1) as integer
  dim as integer ret
  if rnd < .5 then not_value = -1
  do
    dim as single s = rnd
    select case s
    case is < a(0):  ret=0
    case is < a(1):  ret=1
    case is < a(2):  ret=2
    case is < a(3):  ret=3
    case is < a(4):  ret=4
    case is < a(5):  ret=5
    case is < a(6):  ret=6
    case is < a(7):  ret=7
    case is < a(8):  ret=8
    case is < a(9):  ret=9
    case is < a(10):  ret=10
    case is < a(11):  ret=11
    case is < a(12):  ret=12
    case is < a(13):  ret=13
    case else:       ret=14
    end select
  loop while ret = not_value
  return ret
end function

sub norm_squash(a() as single) '' a general-purpose sub
  dim as single s = a(0)
  for i as integer = 1 to ubound(a):  s += a(i):  Next: a(0) /= s
  for i as integer = 1 to ubound(a)
    a(i) = a(i)/s + a(i-1)
  Next
End Sub

Function r(b sng=.5, v sng=1) sng
  Return b+rnd*v
End Function

Function m(mul sng=1, b sng=.5) sng
  Return mul*(b+Rnd)
End Function

sub new_weights(byref ev as eval_vars)
  dim as single   time_spent_randomizing(14) = {1,1,5,1,1,1,5,1,1,1,2,2,1,5,5}
                                              
  norm_squash time_spent_randomizing()
  for i as integer = 0 to ubound(time_spent_randomizing) * .35
    select case index_from_probability( time_spent_randomizing() )
                                            '' ranges become more obvious w/ experience
    #if 0
     case 7:  ev. parm7 = r(0,.8)         'blur f0        - blur wait frame
     case 12:  ev. parm12 = m(.1,.5)      'qexp          - enforced sampling decay
    #else
     case 7:  ev. parm7 = r(.1,.9)        'blur f0        - blur wait frame
     case 12:  ev. parm12 = m(.025,1)      'qexp          - enforced sampling decay
    #endif
    
    '' r: a + rnd*b
    '' m: a*(b+rnd)
      case 0:  ev. parm0 = r(.005,.4)       'srate          - importance map threshold telling a pixel to render
      
      '' The intent behind sparse sampling is to find creative caustics distribution metrics (weights).
      '' Naturally if the rate was higher, the overall score will be greater (in the short term perhaps)
      case 1:  ev. parm1 = r(.045,.0)      'qrate          - enforced sampling density
      
      '' this is a multiplier based upon parameter 9
      case 2:  ev. parm2 =  m(.28,1.5)       'imap fi        - importance map recalculate every i-th frame
      case 3:  ev. parm3 = r(.76,.05)       'imap sus        - importance map sustain
      case 4:  ev. parm4 = (pi/2-1)/4       'imap           - star filter coefficient
      case 5:  ev. parm5 = (pi/2-1)/4       'blur           - blur amount
      case 6:  ev. parm6 = r(.2,1.1 )        'blur fi        - blur every i-th frame
      case 8:  ev. parm8 = m(.8,2.0)         'grid fi         - grid jitter every i-th frame
      case 9:  ev. parm9 = 12'm(7 , 2)       'ipix_trig      - baseline for frame trigger, blur_f0, blur_fi, imap_fi
      case 10:  ev. parm10 = m(35, 1.5)      'ipix trig mul  - pixel trigger increment
      case 11:  ev. parm11 = r(.7, .6)      'ipix trig exp  - pixelsize decay
      case 13:  ev. parm13 = r(.05,.65)      'blur_thresh  - blur if importance >=
      case 14:  ev. parm14 = m(.005,1)      'blur_exp     - blur decay
    end select
  next
end sub


sub render_test(byref tr as tracer ptr, byref sc as scene ptr, x as ushort = 0, y as ushort = 0)
  var r_time = 20
  var update_interval = .15
  dim as double t, t_info=t, tsum
  while tsum < r_time
    var t0 = timer
    tr->render *sc
    t = timer:  tsum += t-t0
    if t >= t_info then
      t_info = t + update_interval
      sc->image_out x, y
      screenlock: screenunlock
      dim as string s="render test " & round((tsum)/60,2) & " / " & round(r_time/60,2)
      windowtitle s
      sleep 1
    endif
    if inkey<>"" then exit while
  wend
  sc->image_out x,y
  screenlock: screenunlock
end sub


function calc_score(sce as scene ptr) as double
  
  'One pixel of high importance will hog most of cpu.
  'A simple score calculation could well be:  pixels iteration sum ..
  'i.e., the more spread out the importance, the better the result
  
  var score = 0!
  with *sce
    'var mult = 1 / (.ub+1)'# 'double, 
    for i as integer = 0 to .u
      score += .a(i).a.iter '* mult
    next:  end with
  return score '/ (sce->ww * sce->hh)
End function


dim shared as double gdt

sub evaluate_render(byref evs as tEVS, byref tr as tracer ptr, byref sc as scene ptr)
  
  var update_interval = .9
  var t = timer, t0 = t
  var t_info = t + update_interval
  var r_time = gdt
  
  var border = 2, xw = sc->w+border, x0=xw*0
  
  do
    tr->render *sc
    t = timer
    if t >= t_info then
      t_info = t + update_interval
      sc->image_out x0, 0
      'locate 22, 1: ? sc->frame, sc->pixel_size, sc->pix_size_frame_trig; "  "
      screenlock: screenunlock
      sleep 1
      evs.kstr = inkey
      if evs.kstr<>"" then exit do
    endif
    r_time -= t - t0
    t = t0:  t0 = timer
  loop while r_time > 0
 
  if gdt < update_interval then  cls:  sc->image_out x0, 0
  
  evs.s = calc_score(sc)
  sc->dimensions
end sub


' ------ scene -----------


const BASE_RAD = 50

dim shared as tMatte    mat
dim shared as single    ior
 
sub sphere_as_ground(byref sce as scene, col as v3=type(1,1,1), k as single=.1, fresnel as single=0)
  mat.quick_solid col, k, fresnel, 0
  sce.add_sphere 0,-BASE_RAD,0, BASE_RAD, sce.add_material(mat.refl, mat.refr, mat.em)
End sub

sub create_lights(byref sce as scene, num as integer=3, brightness as single=1, size_min as single = 1, y as single = .5, size_variance_mult as single = 2.5)
  var size_avg = size_min * (1+size_variance_mult) / 2
  for i as integer = 1 to num
    var a = i / num
    mat.quick_light hsv(a*6,.5,1)*brightness
    var rad = size_min + rnd * size_variance_mult*size_min, dist=.8, angle = twopi*a
    dim as v3 v=type( dist*cos(angle), y, dist*sin(angle) )
    sce.add_sphere v.x,v.y,v.z, rad, sce.add_material(mat.refl, mat.refr, mat.em)
  next
End Sub

sub mat_random
    var k = rnd*rnd, fresnel = 2*(rnd-.5), refr_amt = rnd: refr_amt=1
    mat.quick_solid hsv(rnd*6,rnd*rnd,.67*(.5+rnd)), k, fresnel, refr_amt
    ior = 1.05 + rnd*.67
End Sub

sub create_spheres(byref sce as scene, num as integer = 10, size_min as single = .02, variance_mult as single = 1)
  dim as single spread = 1-cos(pi/270)
  dim as v3 yvec = type(0,1,0)
  for i as integer = 0 to num-1
    mat_random
    var rad = size_min * (1 + rnd * variance_mult)
    dim as v3 v=yvec.rand((rnd)*spread)*(base_rad+rad)
    sce.add_sphere v.x,v.y-base_rad,v.z, rad, sce.add_material(mat.refl, mat.refr, mat.em, ior)
    for j as integer = 0 to -1'+int(rnd*rnd*1.5)
      mat_random
      sce.pobjects->obj(sce.pobjects->ub_obj).desc.mat_layer sce.add_material(mat.refl, mat.refr, mat.em, ior)
    Next
  next: ?
end sub


sub Main

  dim as imagevars  buf
  
  buf.screen_init 640,480
  

  /' ----- scene with a small, bright emitter.  firefly heaven.  '/
  
  var seed = 3

  for i as long = 1 to seed
    dim as single f = rnd
  Next
  
  var image_scale = .15
  dim as scene      sc=type(buf.w*image_scale,buf.h*image_scale)
  
  sc.pobjects->bgcol = hsv(.7,.17,.18)
  
  sphere_as_ground sc, hsv(.5,0,.8)*2 - sc.pobjects->bgcol, 0.4, .0
 
  dim as tMatte mat
  mat.quick_solid hsv(0,0,.8)       ,.00, .5
  
  '                   num, brightness, size_min,    y, size_vari_mult
  create_lights sc,   5,         210,     .004,  .35
  
  '                   num, size_min, size_vari_mult
  create_spheres sc, 48,    .0023,              9
  
  mat.quick_light hsv(-.15, .6, 1) * 12500
  
  '             x  y  z   rad
  sc.add_sphere 0,.02,0, .004, sc.add_material(mat.refl, mat.refr, mat.em)
 
  var a = .577
 
  sc.pobjects->obj(sc.pobjects->ub_obj).desc.ori.x = -.38 + 0.6*a
  sc.pobjects->obj(sc.pobjects->ub_obj).desc.ori.z = 0.1 + .2*a
  
  sc.pobjects->cam.zoom = 1.1
 
  var pos_scalar = .6
  sc.pobjects->cam.o.z = (-2.5 + .5*a) * pos_scalar
  sc.pobjects->cam.o.y = (.4 - .1*a) * pos_scalar
  sc.pobjects->cam.o.x = (-0.1 + 1.3*a) * pos_scalar
  
  sc.look_at .02, -.05, .00
  '
  ' ---- scene end -------
 
  sc.render_target buf 
  var sc0 = sc, sc1 = sc

  'image_scale = .25
  sc0.dimensions buf.w * image_scale, buf.h * image_scale
  sc1.dimensions sc0.w, sc0.h

  sc.scene_to sc0
  sc.scene_to sc1
 
  dim as tracer   tr

  'render_test @tr, @sc0, sc.w+2 + sc0.w+2
  ? "done.":  ?
  ? "next, weights adjustment"
  'sleep 1400

  dim as eval_vars  weights_best = gEV
  dim as tEVS       best, temp
  temp.p = @gEV
  best.p = @weights_best
  
  #define b *temp.p      '' part 1/2 - make changes here when u craft a new weight
  
  dim as string strfile = exepath & "\" & "weights.txt"
  
  var min_time = image_scale * 0.5  '' 1. start with short render
  'var max_time = min_time * 75       '' 2. gradually increased render times 
  gdt = min_time
  
  For runs As Long = 1 To 11
  'while gdt <= max_time
    dim as string     kstr
    var search_count = 3.4 / gdt
    for test_pass as integer = 1 to 3.65 + search_count
      windowtitle  "pass "& str(test_pass) & "  best " & str(best.s)
      evaluate_render temp, @tr, @sc
      printout temp, best
      screenlock: screenunlock
      if temp.s > best.s then
        best.s = temp.s
        weights_best = gEV
        if test_pass > 1 then
        endif
      else
        gEV = weights_best
      endif
      sleep 1
      kstr = inkey
      if temp.kstr <> "" then kstr = temp.kstr
      if kstr <> "" then exit for
      new_weights gEV
    next
    printout temp, best
    gEV = weights_best
    best.s = 0
    if kstr <> "" then exit for
    gdt *= 1.4
  next

          open strfile for output as #1
            write #1, "srate", b. parm0
            write #1, "qrate", b. parm1
            write #1, "imap fi", b. parm2
            write #1, "imap sus", b. parm3
            write #1, "imap star filter", b. parm4
            write #1, "blur star filter", b. parm5
            write #1, "blur fi", b. parm6
            write #1, "blur f0", b. parm7
            write #1, "grid fi", b. parm8
            write #1, "ipix trig", b. parm9
            write #1, "ipix trig mul", b. parm10
            write #1, "ipix trig exp", b. parm11
            write #1, "qexp", b. parm12
            write #1, "blur thresh", b. parm13
            write #1, "blur exp", b. parm14
          close #1
  
  image_scale = .4
  sc1.dimensions buf.w * image_scale, buf.h * image_scale
  render_test @tr, @sc1, sc0.w+2

  draw string(0,sc.h+11), "done!"
  windowtitle "done!  (seed) " & str(seed)
 
  sleep
 
end sub

Main
/'
[code][/code]
pathtracer w.bas
[code][/code]
'/
