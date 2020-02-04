'#include "../rng.bas"

'#undef rnd
'#define rnd csg_3

'      fstcw word ptr [oldcw]

#include "pathtracer x.bas"

'#include "util.bas"

'#undef rnd
'#define rnd csg

' ------- material demo

const BASE_RAD = 50

dim shared as tMatte mat
 
sub sphere_as_ground(byref sce as scene, col as v3=type(1,1,1), k as single=.1, fresnel as single=0)
  mat.quick_solid col, k, fresnel, 0
  sce.add_sphere 0,-BASE_RAD,0, BASE_RAD, sce.add_material(mat.refl, mat.refr, mat.em)
End sub

sub create_lights(byref sce as scene, num as integer=3, brightness as single=1, size_min as single = 1, y as single = .5, size_variance_mult as single = 2.5)
  var size_avg = size_min * (1+size_variance_mult) / 2
  for i as integer = 1 to num
    var a = i / num
    mat.quick_light hsv(a*6,.2,1)*brightness
    var rad = size_min + rnd * size_variance_mult*size_min, dist=.8, angle = twopi*a
    dim as v3 v=type( dist*cos(angle), y, dist*sin(angle) )
    sce.add_sphere v.x,v.y,v.z, rad, sce.add_material(mat.refl, mat.refr, mat.em)
  next
End Sub


sub Main
 
  var seed = 6

  for i as long = 1 to seed
    dim as single f = rnd
  Next
  
  dim as imagevars buf:  buf.screen_init 800, 600
  dim as imagevars buf2: buf2.create buf.w,buf.h
  
  dim as scene    sce0, sce1, sce2
  
  var scale=.333/1.0:  sce0.dimensions buf.w*scale, buf.w*scale  
  sce0.render_target buf2
  
  with *sce0.pobjects
    .bgcol = type(.7,.6,.55)*.6
    
    .cam.zoom = 1.15
   
    var pos_scalar = .7
    .cam.o.z = -2.0 * pos_scalar
    .cam.o.y = .45 * pos_scalar
    .cam.o.x = -.8
  end with
  sce0.look_at .02, .06, 0
 
  dim as tracer   tr

  ' scene ===========
  
  '                                    k, fresnel
  sphere_as_ground sce0, hsv(0,0,.8), .3, .5
  
  var hue = 0, k = 1/25, fres = 0f, refr_amt = 0f, ior = 1f
  k = 1/25
  mat.quick_solid hsv(0,0, .8), k, fres, refr_amt
  sce0.pobjects->obj(sce0.pobjects->ub_obj).desc.mat_layer sce0.add_material(mat.refl, mat.refr, mat.em, ior), .5 ''layer strength
  
 
  '                   num, brightness, size_min,    y, size_vari_mult
  create_lights sce0,   5,         30,     .010,  .35
 
  sce1 = sce0
  mat.quick_solid hsv(0,0,.8)       ,.01, .5
  sce1.pobjects->obj(sce1.pobjects->ub_obj).desc.mat_layer sce1.add_material(mat.refl, mat.refr, mat.em), .3
  sce1.pobjects = @sce1.objects
  
  sce2 = sce1
  sce2.pobjects = @sce2.objects
  
 
  ' material tester ===========
 
  var rad = .1, y=rad+.0
 
  hue = 4: k = 1/25: fres = 0f: refr_amt = 0f: ior = 1f
  mat.quick_solid hsv(hue,.3, 1), k, fres, refr_amt
  sce0.add_sphere 0,y,0, rad, sce0.add_material(mat.refl, mat.refr, mat.em, ior)
  'sce0.pobjects->obj(sce0.pobjects->ub_obj).desc.mat_layer sce0.add_material(mat.refl, mat.refr, mat.em, ior), .5 ''layer strength
 
  mat.refl.fresnel = 1
  sce1.add_sphere 0,y,0, rad, sce1.add_material(mat.refl, mat.refr, mat.em, ior)

  '' layered
  k = 0
  mat.quick_solid hsv(hue, .4, 1), k, mat.refl.fresnel, refr_amt
  sce2.add_sphere 0,y,0, rad, sce2.add_material(mat.refl, mat.refr, mat.em, ior), .7 ''layer strength
 
  refr_amt = 1:  ior = 1.22
  mat.quick_solid hsv(.5, .3, 1), k,                 0, refr_amt, 0 '' refr_amt, light
  sce2.pobjects->obj(sce2.pobjects->ub_obj).desc.mat_layer sce2.add_material(mat.refl, mat.refr, mat.em, ior), .5 ''layer strength
 
  var border=2

  dim as double t=timer, t_next = t, t0=t
  'randomize
  
  while 1
    
    locate 1,1 ''print statements:  upper left corner
    
    'sce0.pobjects->mat_(0).o.refl.k = rnd
    tr.render sce0
    tr.render sce1
    tr.render sce2
    t = Timer

    if t >= t_next then
      var xw = sce0.w+border, x0=xw*0, x1=xw*1, x2=xw*2, y2=sce0.h+31:  y=sce0.h+1
      var y4 = y2 - 10
      
      sce0.image_out x0, 0
      sce0.image_out x0, y2,,, true
      sce1.image_out x1, 0
      sce1.image_out x1, y2,,, true
      sce2.image_out x2, 0
      sce2.image_out x2, y2,,, true
      
      put (0,0), buf2.im, pset
      draw string (x0, y), "simple reflection"
      var matref = sce1.pobjects->obj(sce1.pobjects->ub_obj).desc.msv(0).id
      draw string (x1, y), "fresnel = " & str(sce1.pobjects->mat_(matref).o.refl.fresnel)
      draw string (x2, y), "k = " & str(mat.refl.k) & ", + new mat layer"
      draw string(0,y4), "importance map"
      if sce1.pixel_size <= 4 then windowtitle "frame: " & str(sce1.frame) & " time: " & str( (t-t0)/60 ) & " min"
      t_next = t + 1.2 / sce1.pixel_size ^ 1.1
    endif
    sleep 1
    
    if InKey = Chr(27) then exit while
  wend

  draw string(0,sce0.h+11), "done!"
'  windowtitle "done! " & round( (t-t0)/60, 2 ) & " minutes"
 
  sleep
 
end sub

Main
