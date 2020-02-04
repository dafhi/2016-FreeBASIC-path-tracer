/' "pathtracer.bas" - by dafhi - 2018 Nov 16

+------+ --------------------------------------------------------------------------------------- +
| Info |                            progressive path tracer                                      |
+------+ --------------------------------------------------------------------------------------- +

- unique features -

tone map   = radiance / iter (float)
importance = importance * sustain + frame color delta / iter

-- caustics promotion --
1. progressive resolution

2. blur  [sub:  tracer.blur_imap_and_col]
  - strength per pixel from importance map
  - independently of each other
  A. render
  B. (blur and update) importance map
  
  
update:  sphere intsersect



+ =================== + ============================================================================ + 
| project inspiration |
+ =================== +

Since childhood I have loved visual arts, with cartoons and computer graphics at the forefront.
Seeing a particular image sometime around 2007 struck a deep chord within me about the next generation
of ray tracers, global illumination renderers.  The image in question is DeadMau5's mouse-head logo
emitting a soft light upon its minimalist environment.

Up until late 2015 I'd been working mostly with 2d graphics and basic 3d point plotting.  While I had
a, ahem, solid understanding of the 3d math I had developed, (equivalent to standard formulae) I felt
outclassed by the thought of algorithms which I truly admired.  At that time, I experienced a push to
work on my own ray tracer, and in a couple of months, I had one.

Fast forward to November 2016, I reopened a framework project I'd saved after my first raytracer,
and began work on a path tracer.

After making a few code structure adjustments and fixing several noobie-crafted segments, I saw my basic
no-surprises rendering construct emerge.

'/

#undef rnd

function rnd as double
  static as ulong a, b
  a *= a
  a xor= b
  b += 1:  return a / culng(-1)
End Function

#undef int
#define int as integer


type myint              as integer

'dim shared as Halton    hal = 2, hal2 = 3

'' -- image variables type - 2018 June 29
type imagevars
  as integer            w,h,bpp,bypp,pitch,rate,  wm, hm, pitchBy, num_pages, flags 'helpers
  as any ptr            im, pixels
  as ulong ptr          p32
  as string             driver_name
  declare sub           get_info(im as any ptr=0)
  Declare Sub           screen_init(wid As myint=-1, hgt As myint=-1, bpp as myint=32, numPages as myint=1, Flags as myint=0)
  declare sub           create(w as myint=0, h as myint=0, col as ulong=&HFF000000)
  declare               destructor
  declare sub           release
  as single             midx, midy, diagonal '2017 Oct 10
  'as any ptr            hRelease
end type
Destructor.imagevars:  release
End Destructor
sub imagevars.release                             '2016 Aug 30
  w=0: h=0: bpp=0: bypp=0: im=0: pixels=0
  If ImageInfo(im) = 0 Then ImageDestroy im
  im = 0
End Sub
sub imagevars.get_info(im as any ptr)
  if im=0 then
    pixels=screenptr:  ScreenInfo w,h, bpp, bypp, pitch, rate, driver_name
  elseif Imageinfo(im)=0 then
    ImageInfo im, w, h, bypp, pitch, pixels
    bpp = bypp * 8:  this.im = im
  endif:  pitchBy=pitch\bypp
  wm=w-1: midx=w/2:  diagonal = sqr(w*w+h*h)
  hm=h-1: midy=h/2:  p32=pixels
end sub
sub imagevars.create(w as myint, h as myint, col as ulong)
  release:  get_info imagecreate(w,h,col)
End Sub
Sub imagevars.screen_init(w As myint, h As myint, _bpp as myint, _pages as myint, _flags as myint)
  release:  ScreenRes w,h,_bpp,_pages,_flags: get_info
  num_pages=_pages: flags=_flags
End sub
  

Type stackvars
  as myint            used=-1, ub
  As Single             expansion_coeff = 1.5
  declare function      expand as myint
End Type
function stackvars.expand as myint
  used += 1:  If used>=ub Then ub=used*expansion_coeff+2: return ub
  return 0
End function

function clamp(in as single, hi as single=1, lo as single=0) as single
  if in>hi then return hi
  if in<lo then return lo
  return in
end function

function arymax(a() as single) as single    ' 2017 Feb 19
  dim as single max = a(0):  for i as myint = 1 to ubound(a)
    if max < a(i) then max=a(i)
  next: return max
end function


const as single eps=1e-4  'new:  previously 1e-5 .. reduced resolution could indicate a bug in reflection blur
const as single inf=1e9f

#ifndef pi
const   TwoPi = 8*atn(1)
const   Pi = 4*atn(1)
const   piBy2 = 2*atn(1)
const   piBy4 = 4*atn(1)
#EndIf

type v3
  as single         x,y,z
  declare sub       norm
  declare property  len as single
  declare property  len(in as single)
  declare function  dot(byref as const v3 ptr) as single
  declare function  perp(in as single=1) as v3
  declare function  cross(in as v3, slen as single=1) as v3
  declare function  rand(spread as single=1) as v3
  declare function  blend(in as v3, a as single=.5) as v3  '2017 Feb 14
  declare property  n as v3
end type
property v3.n as v3
  dim as single s = x*x+y*y+z*z:  if s<>0 then s=1/sqr(s)
  return type(x*s,y*s,z*s)
End Property
function v3.blend(in as v3, a as single=.5) as v3
  return type( x + a*(in.x-x), y+a*(in.y-y), z+a*(in.z-z) )
end function
property v3.len as single: return sqr(x*x+y*y+z*z): end property
function v3.dot(byref r as const v3 ptr) as single: return x*r->x+y*r->y+z*r->z: end function
sub v3.norm:  dim as single s=1/sqr(x*x+y*y+z*z): x*=s:y*=s:z*=s: end sub
property v3.len(in as single)
  dim as single s = in/sqr(x*x+y*y+z*z): x*=s: y*=s: z*=s
End Property
function v3.perp(in as single) as v3
  if y=0 then return type(0, in, 0)
  var s=z*z+x*x: if s=0 then return type(in,0,0)
  s=in/sqr(s):  return type(z*s, 0, -x*s)
End function
function v3.cross(r as v3, slen as single) as v3
  dim as single xx=y*r.z - z*r.y
  dim as single yy=z*r.x - x*r.z
  dim as single zz=x*r.y - y*r.x
  dim as single s = xx*xx+yy*yy+zz*zz
  if s=0 then return type(0,0,0)
  s=slen/sqr(s):  return type(xx*s,yy*s,zz*s)
End function
operator +(l as v3,r as v3) as v3: return type(l.x+r.x, l.y+r.y, l.z+r.z): end operator
operator *(l as v3,r as single) as v3: return type(l.x*r,l.y*r,l.z*r): end operator
function v3.rand(spread as single) as v3 '2016 Dec 12
  dim as single cosa = 1-spread
  dim as single sina = sqr(1-cosa*cosa):  spread = rnd*twopi
  dim as v3 perp0 = perp(cos(spread)*sina)
  return this*cosa + perp0 + cross(perp0, sin(spread)*sina)
End function
operator -(r as v3) as v3: return type(-r.x, -r.y, -r.z): end operator
operator -(l as v3,r as v3) as v3: return type(l.x-r.x,l.y-r.y,l.z-r.z): end operator
operator /(l as v3,r as single) as v3: dim as single s = 1/r: return type(l.x*s,l.y*s,l.z*s): end operator
operator *(l as single, r as v3) as v3: return type(l*r.x,l*r.y,l*r.z): end operator
operator *(l as v3,r as v3) as v3: return type(l.x*r.x,l.y*r.y,l.z*r.z): end operator


#Define flr(x) _  '' Stonemonkey's floor function
  (((x)*2.0-0.5)shr 1)

function hsv(h as single, s as single, v as single) as v3
  h -= 6*flr(h/6)
  var x = clamp(2 - h - 2*(h-3)*(h>3))
  var y = clamp(h +     2*(h-2)*(h>2))
  var z = clamp(h - 2 + 2*(h-4)*(h>4))
  dim as single ptr  lo=@x, mi=@y, hi=@z
  if *lo > *mi then swap lo, mi
  if *mi > *hi then swap mi, hi
  if *lo > *hi then swap lo, hi
  *lo = v * (*hi - s * (*hi - *lo))
  *mi = v * (*hi - s * (*hi - *mi))
  *hi *= v
  return type(x,y,z)
End function

function c255(in as single) as myint  '2017 Jan 22
  if in<=0 then return 0
  if in>=1 then return 255
  return in*256-.5
end function


Type Axis3D
  As v3                 vX=(1,0,0), vY=(0,1,0), vZ=(0,0,1)
  as v3                 o
End Type


' ----------------------------- pathtracer.bas
'
type tMatComponent
  as v3             col
  as single         k, fresnel, a
  declare sub       kf(k as single=0, fresnel as single=1)
End Type
sub tMatComponent.kf(_k as single, _fresnel as single)
  k=_k: fresnel=_fresnel
End Sub


type tEmission
  as v3             col
  as single         a
  declare sub       vals(h as single=0, s as single=0, v as single=1, a as single=1)
  declare           constructor(h as single=0, s as single=0, v as single=1, a as single=0)
End Type
constructor tEmission(h as single, s as single, v as single, a as single)
  vals h,s,v,a
End Constructor
sub tEmission.vals(h as single, s as single, v as single, _a as single)
  col=hsv(h,s,v): a=_a
End Sub

type tMatte
  as tMatComponent  refl, refr
  as tEmission      em
  as single         ior=1
  declare sub       quick_light(col as v3 = type(1,1,1), _
                                vis as single=1)
  declare sub       quick_solid(col as v3 = type(1,1,1), _
                                rough as single=0, _
                                fresnel as single=1, _
                                refract_amt as single=0, _
                                li as single=0, _
                                vis as single=1)
end type

sub tMatte.quick_solid(col as v3, rough as single, fresnel as single, refract_amt as single, li as single, vis as single)
  refl.a = (1-refract_amt)*vis
  refr.a = refract_amt*vis:  em.a = li*vis
  refl.col = col:  refr.col = col: em.col=col
  refl.kf rough, fresnel
  refr.kf rough, fresnel
End sub

sub tMatte.quick_light(col as v3, vis as single)
  refl.a=0: refr.a=0: em.col=col: em.a=vis
End Sub

dim shared as tEmission gEmi_

type tMatte_
  as tMatte         o
  declare sub       write(refl as tMatComponent, refr as tMatComponent, emi as tEmission=gEmi_, ior as single=1.2)
end type
sub tMatte_.write(refl as tMatComponent, refr as tMatComponent, emi as tEmission, _ior as single)
  o.refl = refl: o.refr = refr: o.em = emi: o.ior = _ior
End Sub


const obTYPE_SPHERE = 2
const obTYPE_PLANE = 3

type MatStackVars
  as integer           id
  as single         prob, ceil
End Type

type tMaterial
  as integer           ub = -1
  as tMatte         m(any)
End Type


type rtObj
  as integer           ub_msv = -1
  as integer           ob_type
  as v3             ori
  as MatStackVars   msv(any)
  as tMaterial ptr  m
  declare sub       mat_layer(byref as integer=0, byref as single = 1, as integer = -1)
end type

sub rtObj.mat_layer(byref mat_ref as integer, byref stren as single, write_slot as integer)

  if write_slot < 0 then
    write_slot = ubound(msv)+1
  else
    write_slot += ubound(msv)+1
  EndIf

  if write_slot > ub_msv then
    ub_msv = write_slot
    redim preserve msv(ub_msv)
  EndIf
 
  msv(write_slot).id = mat_ref
  msv(write_slot).prob = stren
  var sum=0f
  for i as myint = 0 to ub_msv
    sum += msv(i).prob
  Next: if sum=0 then exit sub
 
  var prob_mul = 1/sum
  msv(0).ceil=msv(0).prob*prob_mul
  for i as myint = 1 to ub_msv
    msv(i).ceil = msv(i-1).ceil+msv(i).prob*prob_mul
  Next
End Sub

type rtObj3D
  as rtObj       desc
  as single         r
  as single         r2
  'as v3             rayocen
  declare sub       write(ob_type as integer = obTYPE_SPHERE, origin as v3 = (0,0,0), r as single = 1, layer_strength as single=1, mat_id as integer = -1)
End Type
sub rtObj3D.write(ob_type as integer, ori as v3, _r as single, layer_strength as single, mat_id as integer)
  desc.ob_type=ob_type
  desc.ori=ori: r=_r:  r2 = r*r
  desc.mat_layer mat_id, layer_strength
End Sub


type rt_camera
  as v3             o, d
  as single         zoom = 1, roll
end type

                                  ''''''' ''''''' ''''''' ''''''' ''''''' ''''''' ''''''' '''''''
const as string     HEADER_DPT = "dafhi path tracer  2018 Oct 20                                 " '63 bytes

type scene_header
  as string*63      txt = HEADER_DPT '63 + 1 byte for string end
End Type

type tr_pixel
  as v3             c
  as single         iter
  declare property  convert as ulong
End Type

property tr_pixel.convert as ulong
  dim as v3 cc=c/iter
  return rgb(c255(cc.x),c255(cc.y),c255(cc.z))
end property


#define parm0 srate
#define parm1 qrate       'initial sampling density
#define parm2 imap_fi
#define parm3 imap_sus
#define parm4 imap_star_filter
#define parm5 blur_star_filter
#define parm6 blur_fi
#define parm7 blur_f0
#define parm8 grid_fi
#define parm9 ipix_trig
#define parm10 ipix_trig_mul
#define parm11 ipix_trig_exp
#define parm12 qexp
#define parm13 blur_thresh
#define parm14 blur_exp

type eval_vars
  #if 0
  as single           parm0 = .17   'weights found with a monte-carlo randomizer
  as single           parm1 = .386
  as single           parm2 = .79
  as single           parm3 = .72
  as single           parm4 = .11
  as single           parm5 = .044
  as single           parm6 = .807
  as single           parm7 = .578
  as single           parm8 = .64
  as single           parm9 = 7.7
  as single           parm10 = 7.8
  as single           parm11 = 1.3
  as single           parm12 = .072
  as single           parm13 = .076
  as single           parm14 = .033
  #elseif 0
  as single           parm0 = .7
  as single           parm1 = .213
  as single           parm2 = .988
  as single           parm3 = .7
  as single           parm4 = .108
  as single           parm5 = .028
  as single           parm6 = .081
  as single           parm7 = .143
  as single           parm8 = .95
  as single           parm9 = 9.33
  as single           parm10 = 4.19
  as single           parm11 = 1.38
  as single           parm12 = .053
  as single           parm13 = .125
  as single           parm14 = .053
  #else
  as single           parm0 = .22
  as single           parm1 = .272
  as single           parm2 = .805
  as single           parm3 = .784
  as single           parm4 = .108
  as single           parm5 = .031
  as single           parm6 = .667
  as single           parm7 = .485
  as single           parm8 = .38
  as single           parm9 = 9.45
  as single           parm10 = 1.95
  as single           parm11 = 1.33
  as single           parm12 = .054
  as single           parm13 = .216
  as single           parm14 = .055
  #endif
end type

dim shared as eval_vars gEV

sub load_weights  
  dim as eval_vars i
  #define b i.
  dim as string tmp, strfile = exepath & "\" & "weights.txt"
  open strfile for input as #1
    if lof(1) < 1 then close #1: exit sub
    input #1,tmp, b parm0
    input #1,tmp, b parm1
    input #1,tmp, b parm2
    input #1,tmp, b parm3
    input #1,tmp, b parm4
    input #1,tmp, b parm5
    input #1,tmp, b parm6
    input #1,tmp, b parm7
    input #1,tmp, b parm8
    input #1,tmp, b parm9
    input #1,tmp, b parm10
    input #1,tmp, b parm11
    input #1,tmp, b parm12
    input #1,tmp, b parm13
    input #1,tmp, b parm14
  close #1
  if i. parm2 <> 0 then gEV = i
end sub

load_weights

type scene_objects
  as rtObj3D          obj(any)
  as tMatte_          mat_(any)
  as v3               bgcol = type(.63, .58, .49)
  as rt_camera        cam
  as myint            ub_obj = -1, ub_mat = -1
  as scene_header     hdr
End Type

type scene_pixel
  as tr_pixel           a, a2
  as integer               clo
  as ulong              colp
  'as v3                 colp
End Type

type clipper_vars
  as single             des0, des1, desd
  as single             src0, src1, srcd
  as myint              isrc
End Type

type scene
  as scene_objects      objects
  as scene_objects ptr  pobjects
  as myint              w=400,h=300
  as double             render_time
  as myint              frame
  as scene_pixel        a(any)
  as single             pr(any)
  as single             dc(any)
  as single             dc2(any)
  as myint              u
  as single             diagonalm, temp
  as single             jit_x, jit_y
  as single             imap_min, imap_max
  as single             imap_f, blur_f, grid_f
  as single             pixel_size, blur_fi, imap_fi
  as single             blur_f0
  as myint              pix_size_frame_trig
  as myint              wm,hm
  as imagevars ptr      imv
  declare sub           scene_to(byref as scene)
  declare sub           render_target(byref imv as imagevars)
  declare function      f_pix_size_trig as single
  declare sub           image_out(as myint=0, as myint=0, as single=0, as single=0, as boolean = false, as boolean = false)
  declare sub           dimensions(as myint=0, as myint=0)
  declare sub           look_at(x as single=0, as single=0, z as single=0)
  declare sub           add_sphere(as single=0, as single=0, as single=0, as single=1, as integer=0, as single=1)
  declare function      add_material(as tMatComponent, as tMatComponent, as tEmission, as single=1) as myint
  declare constructor(as integer=-1, as integer=-1)
  as clipper_vars       clip_x, clip_y
  declare sub           up_scale(as single, as single, as single=0, as single=0)
  declare sub           clipper_exo(as clipper_vars ptr, as single, as single, as myint, as myint)
  as myint              ww,hh,pix_size_trig_inc=500
 private:
  declare sub           dims(as myint, as myint)
  declare sub           reset_pixsizetrig
end type

constructor scene(w as integer, h as integer)
  pobjects = @objects
  pobjects->cam.o.z = -1:  look_at 0,0,0: dimensions w,h
end constructor

sub scene.render_target(byref _imv as imagevars)
  imv=@_imv

End Sub

sub scene.scene_to(byref des as scene)
  des.pobjects = @objects
End Sub

sub scene.reset_pixsizetrig
  pix_size_frame_trig = 0
End Sub

sub scene.dims(_w as myint, _h as myint)
  ww = _w:  if ww<1 then ww=1
  hh = _h:  if hh<1 then hh=1
  wm = ww-1
  hm = hh-1
  u = ww*hh-1
  diagonalm=sqr(wm*wm+hm*hm)
  redim a(u)     'vector col
  redim dc(u)    'delta col
  redim dc2(u)   'normalize
  redim pr(u)    'probability
  frame = 0
  imap_f = 2
  blur_f = 2
  grid_f = 1
  reset_pixsizetrig
  pix_size_frame_trig = f_pix_size_trig
End Sub

sub scene.dimensions(_w as myint, _h as myint)
  if _w <1 then _w = w
  if _h <1 then _h = h
  w = _w
  h = _h
  pixel_size = 32
  dims w/pixel_size, h/pixel_size
  wm = w - 1
  hm = h - 1
End Sub

function scene.add_material(refl as tMatComponent, refr as tMatComponent, emit as tEmission, ior as single) as myint
  with *pobjects
    .ub_mat+=1:  redim preserve .mat_(.ub_mat)
    .mat_(.ub_mat).write refl, refr, emit, ior
    return .ub_mat
  end with
end function

sub scene.look_at(x as single, y as single, z as single)
  pobjects->cam.d = (type(x,y,z)-pobjects->cam.o)
End Sub
sub scene.add_sphere(x as single, y as single, z as single,r as single, mat_id as integer, layer_strength as single)
  with *pobjects
    .ub_obj+=1:  redim preserve .obj(.ub_obj)
    .obj(.ub_obj).write obTYPE_SPHERE, type(x,y,z), r, layer_strength, mat_id
  end with
end sub

sub scene.clipper_exo(xy as clipper_vars ptr, des_xy as single, blit_wh as single, src_wh as myint, des_whm as myint)
    xy->des0 = des_xy + des_xy * (des_xy < 0)
    xy->des1 = des_xy + blit_wh - 1
    xy->des1 += (xy->des1 - des_whm) * (xy->des1 > des_whm)
    xy->desd = xy->des1 - xy->des0

    xy->src0 = (xy->des0 - des_xy) * src_wh / (blit_wh-1)  'cannot multiply by src_step.  these need precision.
    xy->src1 = (xy->des1 - des_xy) * src_wh / (blit_wh-1) - .0001
    xy->srcd = xy->src1 - xy->src0
End Sub

sub scene.image_out(x as myint, y as myint, blit_w as single, blit_h as single, importance_map as boolean, flipRB as boolean)
  
    if blit_w < 1 then blit_w = w
    if blit_h < 1 then blit_h = h
    clipper_exo @clip_x, x, blit_w, ww, imv->wm
    clipper_exo @clip_y, y, blit_h, hh, imv->hm
    
    var sx = 0f, sy = 0f
    dim as long col()
    if not importance_map then:  redim col(u)
      for x = 0 to u:  col(x) = a(x).a.convert
      Next
    endif
    
    for y = clip_y.des0 to clip_y.des1
      sy                 = (y-clip_y.des0) / clip_y.desd
      clip_y.isrc        = flr(clip_y.src0 + sy * clip_y.srcd)
      dim as long ptr pd = imv->p32 + y * imv->pitchBy
      if importance_map then
        dim as single ptr ps = @pr(0) + clip_y.isrc * ww
        for x = clip_x.des0 to clip_x.des1
          sx = (x-clip_x.des0) / clip_x.desd
          clip_x.isrc = flr(clip_x.src0 + sx * clip_x.srcd)
          pd[x] = c255(ps[clip_x.isrc])*(1+256+65536)
        next
      else
        dim as long ptr ps = @col(0) + clip_y.isrc * ww
        dim as long c
        if flipRB then
          for x = clip_x.des0 to clip_x.des1
            sx = (x-clip_x.des0) / clip_x.desd
            clip_x.isrc = flr(clip_x.src0 + sx * clip_x.srcd)
            c = ps[clip_x.isrc]
            pd[x] = (c and &HFF0000)shr 16 or c and &HFF00 or (c and &HFF)shl 16
            '*p=(col and &HFF0000)shr 16 or col and &HFF00 or (col and &HFF)shl 16
          next
        else
          for x = clip_x.des0 to clip_x.des1
            sx = (x-clip_x.des0) / clip_x.desd
            clip_x.isrc = flr(clip_x.src0 + sx * clip_x.srcd)
            pd[x] = ps[clip_x.isrc]
          next
        EndIf
      EndIf
    next
end sub

sub scene.up_scale(des_w as single, des_h as single, des_x as single, des_y as single)
  
  dim as scene_pixel  a2(u)
  dim as single       pr2(u)
  
  for i as myint = 0 to u
    a2(i) = a(i)
    pr2(i) = pr(i)
    dc2(i) = dc(i)
  Next
  if des_w<1 then des_w=1
  if des_h<1 then des_h=1
  des_w = flr(des_w+.5)
  des_h = flr(des_h+.5)
  clipper_exo @clip_x, des_x, des_w, ww, imv->wm
  clipper_exo @clip_y, des_y, des_h, hh, imv->hm

  var _w = ww, sx = 0f, sy = 0f
  dims des_w, des_h
  for y as myint = clip_y.des0 to clip_y.des1
    sy                 = (y-clip_y.des0) / clip_y.desd
    clip_y.isrc        = flr(clip_y.src0 + sy * clip_y.srcd)
    var ides = y * ww
    var isrc = clip_y.isrc * _w
    for x as myint = clip_x.des0 to clip_x.des1
      sx = (x-clip_x.des0) / clip_x.desd
      var si = isrc + flr(clip_x.src0 + sx * clip_x.srcd)
      var di = ides + x
      a(di) = a2(si)
      pr(di) = pr2(si)
      dc(di) = dc2(si)
    next
  next
end sub

function scene.f_pix_size_trig as single
    imap_fi = gEV.imap_fi * gEV.ipix_trig
    blur_fi = gEV.blur_fi * gEV.ipix_trig
    blur_f0 = blur_fi * gEV.blur_f0
    return gEV.ipix_trig * gEV.ipix_trig_mul / (10*pixel_size) ^ gEV.ipix_trig_exp
End function


type tGlobalHelpers
  as v3           o
  as single       b, bb, half_b, c2, c4, c
  as single       disc, sq_disc
  as single       t1, t0
end type

dim shared as tGlobalHelpers  global_helpers



type rt_ray
  as v3             o, d
  as v3             radiance
  declare function  pos(fac as single) as const v3
  declare function  Refract(byref N as v3 ptr, eta as single) as v3
  declare function  sphere_intersect(obj as rtObj3D ptr) as single
end type

function rt_ray.pos(fac as single) as const v3:  return type(o.x+d.x*fac,o.y+d.y*fac,o.z+d.z*fac):  end function

function rt_ray.Refract(byref N as v3 ptr, eta as single) as v3
  dim as single dotNI = N->dot(@d)
  dim as single k = 1.0 - eta * eta * (1.0 - dotNI*dotNI)
  if (k<0.0) then
    return type(0,0,0)
  elseif (k=0.0) then
    return eta*d - eta*dotNI * *N
  else
    return eta*d - (eta*dotNI + sqr(k)) * *N
  end if
end function

'' from my path tracer
function rt_ray.sphere_intersect(obj as rtObj3D ptr) as single
  #define g global_helpers
  #if 0
   g.half_b = d.dot(@g.o)
   g.c = g.o.dot(@g.o) - obj->r2
   if g.c >= g.half_b*g.half_b then return inf
   
   '' one-line finishing move from inigo quilez video
   return -g.half_b - sqr( ( g.half_b*g.half_b - g.c ) )
  #elseif 1
   g.b = d.dot(@g.o)
   g.c = g.o.dot(@g.o) - obj->r2
   if g.c > g.b*g.b then return inf
   return -g.b-sqr( ( g.b*g.b - g.c ) * 4 ) / 2
  #else
   dim as single b = d.dot(@obj->rayocen)
   var c = obj->rayocen.dot(@obj->rayocen) - obj->r2
   if c > b*b then return inf
   dim as single sqrdisc = sqr((b*b-c)*4)/2
   if sqrdisc-b<0 then return inf
   return -b-sqrdisc
  #endif
end function


type trace_stack
  as tMatte_ ptr      mat
  as rt_ray           ray_refl, ray_refr, ray_em
  as v3               hp, norm, nrand, rayd
  as v3               sum_refl, sum_refr, sum_em, v3sumBG, v3sumFG
  as single           fresnel, cosi, fres2, fres2r, imulti
  as myint            closest, multipass, _multipass, i
  as rtObj3D ptr      pobj
end type
 
type file_pixel field = 1
  as ushort           ds, r, g, b
  as ulong            iter65
end type

type tracer
  declare sub         render(byref sce as scene)
  declare sub         clear
  declare constructor
 private:
  declare sub         calc_probability
  declare sub         precalcs(byref sce as scene)
  declare function    norm_scatter(ByRef as v3 ptr, k as single) as v3
  declare sub         grid
  declare sub         camera_to_axis
  declare sub         trace(as rt_ray ptr)
  declare sub         trace_(as rt_ray ptr)
  declare sub         pixel(as myint, as myint = 1)
  declare sub         find_closest(as trace_stack ptr, ray as rt_ray ptr)
  declare sub         frame
  declare sub         blur_pix(as myint, as myint)
  declare sub         blur_imap_and_col
  declare sub         imap_pix(as myint, as myint, des() as single, src() as single)
  declare sub         star_imap(() as single, src() as single)
  declare sub         delta_col(as myint)
  declare sub         star_blur
  as trace_stack      stack(any)
  as scene ptr        sce
  as stackvars        sv
  as axis3D           axis, unit_axis
  as v3               yvec
  as v3               v3temp
  as rt_ray           uray
  as single           least_dist
  as single           srnd, blur_mul, blur_fac
  as myint            depth
  as single           xstart, ystart, xend, yend
end type

constructor.tracer
  redim stack(0)
end constructor

sub tracer.clear
  sce->dimensions
  sce->frame=0: sce->render_time=0
End Sub

sub tracer.precalcs(byref sce as scene)
  for i as myint = 0 to sce.pobjects->ub_obj
    sce.pobjects->obj(i).r2 = sce.pobjects->obj(i).r ^ 2
  Next
  this.sce=@sce
End Sub

sub tracer.camera_to_axis
  var s = .5 / sce->diagonalm
  axis.vz = sce->pobjects->cam.d.n
  axis.vx = axis.vz.perp(s)
  axis.vy = axis.vz.cross(axis.vx, -s)
  axis.vz *= sce->pobjects->cam.zoom
end Sub

sub tracer.find_closest(st as trace_stack ptr, ray as rt_ray ptr)
  st->closest = -1: least_dist = inf
  for i as myint = 0 to sce->pobjects->ub_obj
    select case as const sce->pobjects->obj(i).desc.ob_Type
    case obTYPE_SPHERE
      'sce->pobjects->obj(i).rayocen = ray->o - sce->pobjects->obj(i).desc.ori
      global_helpers.o = ray->o -  sce->pobjects->obj(i).desc.ori
      var dist = ray->sphere_intersect(@sce->pobjects->obj(i))
      if dist>eps andalso dist < least_dist then least_dist=dist: st->closest=i
    end select
  next
end sub

function tracer.norm_scatter(ByRef rayd as v3 Ptr, k as single) as v3

  ' Maths description:
  ' randomize the reflection normal in a cone
  ' at minimum roughness (k=0), cone center is parallel with surface normal. the cone spread is also zero.
  ' at maximum roughness (k=1), cone center points halfway between ray and surf norm, cone spread is 90 degrees.
 
  Static As Single abscosi:  abscosi = Abs(stack(depth).cosi)
  static as v3 perp:      perp = *rayd - stack(depth).norm * abscosi
  static as single angle: angle = atan2(sqr(1-abscosi*abscosi), abscosi)*.5*k
  static as single cosa, sina:  cosa = cos(angle): sina = sin(angle)
  static as v3 newnorm:   newnorm = stack(depth).norm*cosa + perp.n*sina
  'angle = (rnd)*k*piBy4 '' 
  angle = (1-sqr(rnd))*k*piBy4 '' 2017 Dec 25
  cosa=cos(angle):  sina=sin(angle)
  k = rnd*twopi
  perp = newnorm.perp(cos(k)*sina)
  return newnorm*cosa + perp + newnorm.cross(perp, sin(k)*sina) 
end function

sub tracer.trace(ray as rt_ray ptr)
  
  #macro def_imulti(fres, sum)
    sum += .v3sumBG + fres * (.v3sumFG - .v3sumBG)
  #EndMacro
  
  with stack(depth)
    if .cosi > 0 then 'inside sphere
      
      if .mat->o.refr.a<>0 then
        .fres2r = .mat->o.refr.fresnel * .fresnel
        .nrand = norm_scatter(@ray->d, .mat->o.refr.k*(1-.fres2r))
        .ray_refr = type(.hp, ray->refract(@.nrand, .mat->o.ior))
        
        trace_ @.ray_refr
        .v3sumBG = .ray_refr.radiance * .mat->o.refr.col
        .v3sumFG = .ray_refr.radiance
        def_imulti(.fres2r, .sum_refr)
      endif
    
    else
      
      .rayd = -ray->d
      if .mat->o.refr.a<>0 then
        .fres2r = .mat->o.refr.fresnel * .fresnel
        .nrand = norm_scatter(@.rayd, .mat->o.refr.k*(1-.fres2r))
        .ray_refr = type(.hp, ray->refract(@.nrand, 1/.mat->o.ior))
        
        trace_ @.ray_refr
        .v3sumBG = .ray_refr.radiance * .mat->o.refr.col
        .v3sumFG = .ray_refr.radiance
        def_imulti(.fres2r, .sum_refr)
      endif
     
      if .mat->o.refl.a<>0 then
        .fres2 = .mat->o.refl.fresnel * .fresnel
        .nrand = norm_scatter(@.rayd, .mat->o.refl.k*(1-.fres2))
        .ray_refl = type(.hp, ray->d - .nrand*ray->d.dot(@.nrand)*2)
        trace_ @.ray_refl
        .v3sumBG = .ray_refl.radiance * .mat->o.refl.col
        .v3sumFG = .ray_refl.radiance
        def_imulti(.fres2, .sum_refl)
      endif
     
      ' emission ray obtains behind-object color data,
      ' allowing transparency for all rays
      if .mat->o.em.a<>0 then
      .ray_em = type(.hp, ray->d)
      trace_ @.ray_em:  .sum_em += .ray_em.radiance
      endif
     
    endif
  end with
End Sub

sub tracer.trace_(ray as rt_ray ptr)

  depth += 1
  If depth = sv.ub Then
    sv.ub = depth * sv.expansion_coeff + 2
    redim preserve stack(-1 to sv.ub)
  endif
 
  ray->d.norm
  find_closest @stack(depth), ray
  
  with stack(depth)
 
    if .closest >= 0 then
      
      .hp = ray->pos( least_dist )
      
      select case as const sce->pobjects->obj(.closest).desc.ob_Type
      case obTYPE_SPHERE
        .norm = (.hp - sce->pobjects->obj(.closest).desc.ori) / sce->pobjects->obj(.closest).r
      end select
     
      .pobj = @sce->pobjects->obj(.closest)
      srnd=rnd
      for i as myint = 0 to .pobj->desc.ub_msv
        if srnd < .pobj->desc.msv(i).ceil then .mat = @sce->pobjects->mat_(.pobj->desc.msv(i).id): exit for
      Next
   
      ray->radiance = type(0,0,0)
      if depth < 4 then '' depth lbound = 0
        .cosi = ray->d.dot(@.norm)
        .fresnel = clamp(1+.cosi)
        .sum_refl=type(0,0,0) '' 3 rays per trace: reflection, refraction, and emission
        .sum_refr=type(0,0,0)
        .sum_em = type(0,0,0)
        trace ray
        ray->radiance += .sum_em + _
                        .mat->o.refl.a * (.sum_refl - .sum_em) + _
                        .mat->o.refr.a * (.sum_refr - .sum_em) + _
                        .mat->o.em.a * (.mat->o.em.col - .sum_em)'
      else
        
        const int iRoulette = 5
        
        if rnd < 1 / iRoulette then
          
          .cosi = ray->d.dot(@.norm)
          .fresnel = clamp(1+.cosi)
          .sum_refl=type(0,0,0)
          .sum_refr=type(0,0,0)
          .sum_em = type(0,0,0)
          
          const int MULTI_TRACE = iRoulette - 1 '' less than iRoulette or you'll smoke the stack
          
          for i int = 1 to MULTI_TRACE
            trace ray
          Next
          
          ray->radiance += (.sum_em + _
                          .mat->o.refl.a * (.sum_refl - .sum_em) + _
                          .mat->o.refr.a * (.sum_refr - .sum_em) + _
                          .mat->o.em.a * (.mat->o.em.col*2 - .sum_em)) * iRoulette / MULTI_TRACE
        endif
        
      EndIf
    else
      ray->radiance = sce->pobjects->bgcol
    endif
  end with:  depth -= 1

end sub
sub tracer.pixel(i as myint, loops as myint)
  with *sce
    .a(i).colp = .a(i).a.convert
    while loops > 0:  loops -= 1
      uray.radiance = type(0,0,0)
      depth = -1
      trace_ @uray
      .a(i).a.iter += 1
      .a(i).a.c += uray.radiance
    wend:  .a(i).clo = stack(0).closest
  end with
End Sub
function dcol(c0 as ulong, c1 as ulong=0) as single    ' 2018 July 5
  dim as long           a = (c0 and 255) - (c1 and 255)
  dim as long b=a*a:    a =((c0 shr 8)and 255) - ((c1 shr 8)and 255)
  b += a*a:             a =((c0 shr 16)and 255) - ((c1 shr 16)and 255)
  return sqr(b+a*a)
end function
sub tracer.delta_col(i as myint)
    if sce->a(i).clo = stack(0).closest then
      var col = sce->a(i).a.convert
      var s = dcol( sce->a(i).colp, col )'^.4
      if s > sce->imap_max then: sce->imap_max = s
      elseif s<>0 then
        if s < sce->imap_min then sce->imap_min = s
      endif
      sce->dc(i) = sce->dc(i) * gEV.imap_sus + s / sce->a(i).a.iter
      sce->a(i).colp = col
    endif
End Sub


'this section, lots of experimentaion
sub tracer.blur_pix(i as myint, i2 as myint)
    #define f sce->a
    if f(i).clo = f(i2).clo then
      f(i).a2.c += blur_mul * ( f(i2).a.c - f(i).a.c )
      f(i).a2.iter += blur_mul * ( f(i2).a.iter - f(i).a.iter )
    endif
End Sub
sub tracer.star_blur
    with *sce
      
      var div = ( (.frame - .blur_f0) / .blur_fi )
      blur_fac = gEV.blur_star_filter / div ^ gEV.blur_exp
      if blur_fac<0 then blur_fac=0': ? "blur fac <": sleep 10
      if blur_fac>gEV.blur_star_filter then blur_fac=gEV.blur_star_filter': ? "blur fac >": sleep 10
      
      dim as myint  w=.ww, h=.hh, wm=w-1, hm=h-1, i
      
      for y as myint = 0 to hm
        var yw = y*w
        for x as myint = 0 to wm
          i=yw+x
          if .pr(i)>= gEV.blur_thresh then
            .a(i).a2 = .a(i).a
            blur_mul = blur_fac * .pr(i)
            if x>0 then blur_pix i, i-1
            if x<wm then blur_pix i, i+1
            if y>0 then blur_pix i, i-w
            if y<hm then blur_pix i, i+w
          endif
        Next
      Next
      
      for y as myint = 0 to hm
        var yw = y*.ww
        for x as myint = 0 to wm
          i=yw+x
          if .pr(i)>= gEV.blur_thresh then .a(i).a = .a(i).a2
          '.a(i).a = .a(i).a2
        Next
      Next
      
    end with
 
end sub

sub tracer.imap_pix(i as myint, i2 as myint, des() as single, src() as single)
    if sce->a(i).clo <> sce->a(i2).clo then exit sub
    des(i) += blur_mul * (src(i2) - src(i))
End Sub

sub tracer.star_imap(des() as single, src() as single)
  
    with *sce
      
      dim as myint  w=.ww, h=.hh, wm=w-1, hm=h-1, i
      blur_mul = gEV.imap_star_filter
      for y as myint = 0 to hm
        var yw = y*w
        for x as myint = 0 to wm
          i=yw+x
          des(i) = src(i)
          if x>0 then imap_pix i, i-1, des(), src()
          if x<wm then imap_pix i, i+1, des(), src()
          if y>0 then imap_pix i, i-w, des(), src()
          if y<hm then imap_pix i, i+w, des(), src()
        Next
      Next

    end with
 
end sub

sub array_normalize(des() as single, src() as single, mode as myint = 0)
    dim as single max = arymax( src() ):  if max = 0 then exit sub
    max=1/max
    select case mode
    case 0  
      for i as myint = 0 to ubound(des)
        des(i)=src(i)*max
      next
    case 1
      for i as myint = 0 to ubound(des)
        var s = src(i)*max
        des(i)=s*s
      next
    case else
      for i as myint = 0 to ubound(des)
        des(i)= sqr(src(i)*max)
      next
    end select
end sub

sub tracer.blur_imap_and_col
    
    with *sce
      
      var iters = 1
      
      if .frame >= (.imap_f) then
        array_normalize .dc2(), .dc(), 1
        for i as myint = 1 to iters
          star_imap .pr(), .dc2()
          array_normalize .dc2(), .pr()
        next
        array_normalize .pr(), .dc2(), 2
        for i as myint = 0 to ubound(.pr)
          'if .pr(i)<0 or .pr(i)>1 then ? .pr(i): sleep 50
          if .pr(i) < 0 then .pr(i) = 0
          if .pr(i) > 1 then .pr(i) = 1
        Next
        .imap_f = .frame + .imap_fi * log(.frame)
      endif
      
      if (.frame - .blur_f0) >= (.blur_f) then
        for i as myint = 1 to iters
          star_blur
        next
        .blur_f = .frame + .blur_fi * .frame ^ .2
      EndIf
    
    end with

End Sub
sub tracer.grid
    if sce->pixel_size = 1 then
      if sce->frame >= (sce->grid_f) then
        sce->grid_f += gEV.grid_fi * gEV.ipix_trig
        sce->jit_x = (rnd-.5)
        sce->jit_y = (rnd-.5)
      EndIf
    else
      sce->jit_x = 0
      sce->jit_y = 0
    endif
    xstart = -(sce->ww-1)/2 + sce->jit_x
    xend = xstart + sce->ww-1+.1
    ystart = -(sce->hh-1)/2 + sce->jit_y
    yend = ystart + sce->hh-1+.1
end sub

sub tracer.frame

    dim as myint i
   
    with *sce
      
      grid
      
      .frame += 1
      
      if .frame = 1 then
        
        for y as single = ystart to yend:  yvec=axis.vz + axis.vy*y
          for x as single = xstart to xend:  uray.d=yvec+axis.vx*x
            pixel i
            i+=1:  next:  next
        
      else

        if .frame = 2 then
          for y as single = ystart to yend:  yvec=axis.vz + axis.vy*y
            for x as single = xstart to xend:  uray.d=yvec+axis.vx*x
              pixel i
              delta_col i
              i+=1:  next:  next
        else
          dim as single q = gEV.qrate/.frame^gEV.qexp
          dim as single imap_minmax_delt = sqr(.imap_max - .imap_min)
          '.imap_max = 0
          '.imap_min = 2
          for y as single = ystart to yend:  yvec=axis.vz + axis.vy*y
            for x as single = xstart to xend:  uray.d=yvec+axis.vx*x
              if .a(i).a.iter < 999999 then
                if .pr(i) >= gEV.srate then
                  pixel i, .5 + imap_minmax_delt * .pr(i) '' importance map
                  delta_col i
                elseif rnd < q then                       '' brute-force sampling
                  pixel i
                  delta_col i
                endif
              endif
              i+=1:  next:  next
        EndIf
        blur_imap_and_col
      endif
      
      if .pixel_size > 1 then                 ' 2018 July 4
        if .frame >= .pix_size_frame_trig then
          .pix_size_frame_trig = .f_pix_size_trig
          var _pixel_count = .ww * .hh              ' tone remap prep
          .pixel_size /= 2
          .up_scale .w/.pixel_size, .h/.pixel_size
          var s = _pixel_count / (.ww * .hh)        ' prev pixel count / new
          for i as myint = 0 to .u
            .a(i).a.c *= s                          ' remap
            .a(i).a.iter *= s
          Next
        endif
      EndIf
    
    end with
 
End Sub
sub tracer.render(byref _sce as scene)
    precalcs _sce:  camera_to_axis: uray.o = sce->pobjects->cam.o
    dim as double t=timer '2018 July 4
    frame:  _sce.render_time += timer-t
end sub
'
' ----------- pathtracer
