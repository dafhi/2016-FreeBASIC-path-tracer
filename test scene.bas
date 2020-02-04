/' code smashed together for a quick ..

 ----- test scene ------

'/

'#include "rand.bas"

#undef rnd
#define rnd csg_3

#include "pathtracer x.bas"

'' https://www.freebasic.net/forum/viewtopic.php?f=14&t=18491
'#include "../xvidcapture.bi"
  
'#include "util.bas"
function round(in as double, places as ubyte = 0) as string
  dim as integer mul = 10 ^ places
  return str( flr( in * mul +.5 ) / mul )
End Function

'#undef rnd
'#define rnd csg

sub insSort(A() As double,LB As integer=0,UB As integer=-1)
  if lb>ub then lb=lbound(a): ub=ubound(a)
  var lo=lb: for i as integer=1 to ub: if a(i) < a(lo) then lo=i
  next: swap a(lb), a(lo)
  For i as integer=1 To ub-1
    dim as integer j=i+1: if a(j) < a(i) then
      dim as double sw=a(j): j=i: while sw < a(j)
        a(j+1)=a(j): j-=1: wend: a(j+1)=sw: endif: Next
End Sub

sub time_table(a() as double, td as double)
  var ub = ubound(a), mi = (ub+1)\2
  if td <= a(0) then: a(ub) = td
  elseif td >= a(ub) then:  a(0) = td
  elseif a(ub)-a(mi) > a(mi)-a(0) then:  a(ub) = td
  else: a(0) = td
  endif:  inssort a()
End Sub

' ------- material demo
'
  
const BASE_RAD = 50

dim shared as tMatte    mat
dim shared as single    ior
 
sub sphere_as_ground(byref sce as scene, col as v3=type(1,1,1), k as single=.1, fresnel as single=0)
  mat.quick_solid col, k, fresnel, 0
  sce.add_sphere 0,-BASE_RAD,0, BASE_RAD, sce.add_material(mat.refl, mat.refr, mat.em)
End sub

sub create_lights(byref sce as scene, num as integer=3, _
  brightness as single=1, size_min as single = 1, _
  y as single = .5, size_variance_mult as single = 2.5)
  
  #define r rnd
  
  var size_avg = size_min * (1+size_variance_mult) / 2
  for i as integer = 1 to num
    var a = i / num
    mat.quick_light hsv(a*6,.5,1)*brightness
    var rad = size_min + r * size_variance_mult*size_min, dist=.8, angle = twopi*a
    dim as v3 v=type( dist*cos(angle), y, dist*sin(angle) )
    sce.add_sphere v.x,v.y,v.z, rad, sce.add_material(mat.refl, mat.refr, mat.em)
  next
End Sub

sub mat_random
  #define r rnd
  
  var k = r*r, fresnel = r+.1, refr_amt = r: refr_amt=flr(r+.78)'r*r
  mat.quick_solid hsv(r*6,r*r,.67*(.5+r)), k, fresnel, refr_amt
  ior = 1.05 + r*.67
End Sub

sub create_spheres(byref sce as scene, num as integer = 10, size_min as single = .02, variance_mult as single = 1)
  dim as single spread = 1-cos(pi/200)
  dim as v3 yvec = type(0,1,0)
  for i as integer = 0 to num-1
    mat_random
    var rad = size_min * (1 + rnd^1.5 * variance_mult)
    dim as v3 v=yvec.rand((rnd)*spread)*(base_rad+rad)
    sce.add_sphere v.x,v.y-base_rad,v.z, rad, sce.add_material(mat.refl, mat.refr, mat.em, ior)
    for j as integer = 0 to -1'+int(rnd*rnd*1.5)
      mat_random
      sce.pobjects->obj(sce.pobjects->ub_obj).desc.mat_layer sce.add_material(mat.refl, mat.refr, mat.em, ior)
    Next
  next
end sub


sub Main

  var seed = 6

  for i as long = 1 to seed
    rnd
  Next
  
  var base_dim = 440
  'base_dim = 340
'  base_dim = 160
  dim as imagevars  buf:  buf.screen_init base_dim*16/9, base_dim
 
  var image_scale = 1
  dim as imagevars  img:  img.create buf.w*image_scale, buf.h*image_scale
 
  ' scene ===========
  
  dim as scene      sc=type(img.w,img.h)
  sc.pobjects->bgcol = hsv(.77, .27, .80)
  'sc.pobjects->bgcol = hsv(.78, .15, .062)

  sphere_as_ground sc, hsv(.78, .06, .84)*2 - sc.pobjects->bgcol, 0.4, .0
 
  dim as tMatte mat
  mat.quick_solid hsv(0,0,.8)       ,.005, .5
  'sc.obj(0).mat_layer sc.add_material(mat.refl, mat.refr, mat.em), .2
  sc.pobjects->obj(sc.pobjects->ub_obj).desc.mat_layer sc.add_material(mat.refl, mat.refr, mat.em), .12
  
  '                   num, brightness, size_min,   size_vari_mult, y
  'create_lights sc,   5,         100,     .004,  .35
  
  '                   num, size_min, size_vari_mult
  create_spheres sc, 148,    .0011,              19
  
  'mat.quick_light hsv(-.3, .6, 5) * 50
  mat.quick_light hsv(5.7, .9, .3) * 10200
  sc.add_sphere 0,.02,0, .004, sc.add_material(mat.refl, mat.refr, mat.em)
 
  sc.render_target buf
  ' =================
  
  windowtitle "-"
  dim as double tp = timer, times(4)={0,0,0,0,0}', tp0 = tp
  
  ' ========== render settings
  
  chdir exepath
  var file = ""
'  rand_filename file, "avi"
  file = str(seed) + ".avi"

  var anim_fps       = 26
  var anim_seconds    = 14.6
  
  dim as integer frame_end = anim_seconds * anim_fps
 
  dim as integer steppa = 1
  
  dim as single   fm = (frame_end+1)/2
  dim as integer  FE = fm+(fm-1)*steppa
  dim as integer  FS = 1'frame_end+1 - FE
 
  dim as string kstr
  dim as tracer tr
  
  var passes_per_frame = 894
  
'  #define record

  #ifdef record
    const as integer iQuality  = 100
    dim as integer xvid_fps = anim_fps
    'const as integer iChannels =  0
    'const as integer iBits     = 8
    'const as integer iRate     = 11025'22050
    
    '
    if OpenVideoStream(file,buf.w,buf.h,xvid_fps,iQuality)=0 then
    'if OpenVideoAudioStream("test03.avi",w,h,iFPS,iQuality,iChannels,iBits,iRate)=0 then
      print "error: OpenVideoStream() !"
      beep:sleep:end 1
    end if
    '/
    
    for frame as integer = fs+steppa*0 to Fe step steppa*1
  #else
    for frame as integer = 89 to 89
  #endif
    
    var a = frame / 360
   
    sc.pobjects->obj(sc.pobjects->ub_obj).desc.ori.x = -.38 + 0.6*a
    sc.pobjects->obj(sc.pobjects->ub_obj).desc.ori.z = 0.1 + .2*a
    
    sc.pobjects->cam.zoom = 1.1
   
    var pos_scalar = .55
    sc.pobjects->cam.o.z = (-2.5 + 1.0*a) * pos_scalar
    sc.pobjects->cam.o.y = (.33 - .10*a) * pos_scalar
    sc.pobjects->cam.o.x = (-0.1 + 1.3*a) * pos_scalar
    
    sc.look_at .02 - .3*a, -.05, .00 + 1.2*a
   
    var update_mul = 7
    
    dim as double t=timer, t_info=t, tsum
    dim as integer k
    while k < passes_per_frame
      var t0 = timer
      tr.render sc
      t = timer:  tsum += t-t0
      if t >= t_info then
        sc.image_out
        t_info = t + update_mul / sc.pixel_size ^ 1.1
        locate 1,1
        dim as string s = "frame time (mins): " & round((tsum)/60,1)
        #ifdef record
          ? "Recording animation"
        #EndIf
        screenlock:  screenunlock
        windowtitle round(k / passes_per_frame,2)
        sleep 1
      endif
      kstr = inkey
      if kstr<>"" then exit while
      k += 1
    wend
    
    #ifdef record
      #if 0
        sc.image_out
        var num = format(frame,"000")
        bsave exepath & "\bmp\" & num & ".bmp", 0
      #else
        var flipRB = true
        sc.image_out ,,,,,flipRB

        select case buf.bpp
        case 32 : Write32BitPixels(ScreenPtr)
        'case 16 : Write16BitPixels(ScreenPtr)
        end select
        sc.image_out
      #endif
    #else
      sc.image_out
    #endif
    
    screenlock:  screenunlock
    
    if kstr<>"" then exit for
    sleep 1
    
    tr.clear
    
    time_table times(), t-tp
    tp = t
    t_info = t + update_mul / sc.pixel_size ^ 2
    
    var s = (FE - frame) / (FE - FS)
    var pct = flr((1-s)*100 + .5)
    var mid_elem = (ubound(times)+1)\2, td = times(mid_elem)
    var time_remain = (frame_end-1)*s*td/3600
    time_remain = flr(time_remain * 100 + .5) / 100
    windowtitle "frame " & str(frame) & " ... " & str(pct) & "% .. " & str(tsum) & " render time"
    
  next
  
  #ifdef record
  CloseStream()
  #endif
  draw string(0,80), "done!"
  
  sleep
 
end sub

/'
dim as ushort oldcw, cwdouble=&h27F, cwsingle=&h7F
   
asm
  fstcw word ptr [oldcw]
  fldcw word ptr [cwsingle] 'set FPU precision to double
end asm
'/

  Main

'asm      fldcw word ptr [oldcw] 'restore control word
