'#include "rand.bas"

'#undef rnd
'#define rnd csg_3
'#define rnd dodicat_4
'd4_seed 2

#include "pathtracer x.bas"

'#include "util.bas"


sub render_update(byref sc as scene ptr)
  with *sc
    .image_out
    .image_out .w+1,0,,, true ' importance map
  End With:  screenlock:  screenunlock
End Sub


function round(in as double, places as ubyte = 0) as string
  dim as integer mul = 10 ^ places
  return str( flr( in * mul +.5 ) / mul )
End Function

sub Main

  dim as imagevars  buf:  buf.screen_init 800,600
 
  ' ===============
 
  var dims=buf.w*.5 - .5
 
  dim as scene  sce = type(dims,dims)
  dim as tracer tr
  sce.render_target buf
  sce.pobjects->bgcol = hsv(3.65, .16, .89)

  var nRadial = 4
 
  dim as tMatte  mat

  var rad = 3.5/nRadial, refr_amt = 0f, ior=0f
  var z=4, k = 0f, fresnel = .97f, emission = 0f

  for j as integer = 1 to 2
    var a0 = (j-1)/(nRadial*2)
    for i as integer = 1 to nRadial
      var a = i/nRadial + a0
      var angle = twopi * a, dist = 2
      var x=dist*cos(angle), y=dist*sin(angle)
      ior = 1.4
      
      '' material
      k = 1 - cos(pi/2)
      mat.quick_solid hsv(a*6,.7,0.92)*2 - sce.pobjects->bgcol, k,        fresnel, refr_amt, emission
      sce.add_sphere x,y,z, rad, sce.add_material( mat.refl, mat.refr, mat.em, ior ) ''layer strength
     
      '' layered
      k = 0
      mat.quick_solid hsv(0,0,1)*2 - sce.pobjects->bgcol,           k,        fresnel, refr_amt, emission
      sce.pobjects->obj(sce.pobjects->ub_obj).desc.mat_layer sce.add_material( mat.refl, mat.refr, mat.em, ior ), .25 ''layer strength
      
    Next
    z -= 2
  next:  var a=0f
 
  'center material
  ior = 0.5 '' seen if refr_amt is away from zero
  
  k = 1 - cos(pi/5)
  mat.quick_solid hsv(4.05, .4, 0.4)*8 - sce.pobjects->bgcol, k, fresnel, refr_amt
  sce.add_sphere 0,0,3, rad*1.0, sce.add_material( mat.refl, mat.refr, mat.em, ior )
 
  k = 0
  
  #if 0
  mat.quick_solid hsv(4, .5,  .1)*2 - sce.pobjects->bgcol, k, fresnel, refr_amt
  sce.pobjects->obj(sce.pobjects->ub_obj).mat_layer sce.add_material( mat.refl, mat.refr, mat.em, ior ), .8 ''layer strength
  #else
  mat.quick_light hsv(1, .5, 1)*2 - sce.pobjects->bgcol, 1
  sce.pobjects->obj(sce.pobjects->ub_obj).desc.mat_layer sce.add_material( mat.refl, mat.refr, mat.em, ior ), .5 ''layer strength
  #endif
  
  sce.pobjects->cam.o.z = -3.5
  sce.pobjects->cam.zoom = 0.38
 
  var total_seconds = 11

  dim as double t=timer, tsum, t_info=t
  
  #macro show_result()
    render_update @sce
    if sce.pixel_size <= 4 then windowtitle "seconds:  " & round( tsum,0 ) & " / " & round( total_seconds )
  #EndMacro
 
  while tsum < total_seconds
    var t0 = timer
    tr.render sce
    if t >= t_info then
      show_result()
      t_info = t + 2 / sce.pixel_size ^ 1.1
    endif
    sleep 1
    t = timer:  tsum += t-t0
    if inkey<>"" then exit while
  wend
  draw string (0,sce.h+21), "done!"
  show_result()
 
  sleep
end sub


dim as ushort oldcw, cwdouble=&h27F, cwsingle=&h7F
   
   asm
      fstcw word ptr [oldcw]
      fldcw word ptr [cwsingle] 'set FPU precision to double
   end asm

Main

   asm
      fldcw word ptr [oldcw] 'restore control word
   end asm
