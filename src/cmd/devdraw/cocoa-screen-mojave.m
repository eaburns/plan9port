#define Cursor OSXCursor
#define Point OSXPoint
#define Rect OSXRect

#import <Cocoa/Cocoa.h>

#undef Cursor
#undef Point
#undef Rect

#include <u.h>
#include <libc.h>
#include  "cocoa-thread.h"
#include <draw.h>
#include <memdraw.h>
#include <keyboard.h>
#include <cursor.h>
#include "cocoa-screen.h"
#include "osx-keycodes.h"
#include "devdraw.h"
#include "bigarrow.h"
#include "glendapng.h"

AUTOFRAMEWORK(Cocoa)

#define LOG	if(0)NSLog

static void setprocname(const char*);
static uint keycvt(NSEvent *event);
static uint msec(void);
static Memimage* initimg(void);

void resizeimg(void);

void
usage(void)
{
	fprint(2, "usage: devdraw (don't run directly)\n");
	threadexitsall("usage");
}

@interface AppDelegate : NSObject<NSApplicationDelegate,NSWindowDelegate> @end
@interface DevDrawView : NSView @end

static AppDelegate *myApp = NULL;
static DevDrawView *myContent = NULL;
static NSWindow *win = NULL;
static NSCursor *currentCursor = NULL;

static CGColorSpaceRef colorspace = NULL;
static CGContextRef imgContext = NULL;
static CGImageRef imgbuf = NULL;
static int imgbufReady = 0;

static Memimage *img = NULL;

static QLock imgbufl;
static QLock cursorl;
static QLock snarfl;

void
threadmain(int argc, char **argv)
{
	/*
	 * Move the protocol off stdin/stdout so that
	 * any inadvertent prints don't screw things up.
	 */
	dup(0,3);
	dup(1,4);
	close(0);
	close(1);
	open("/dev/null", OREAD);
	open("/dev/null", OWRITE);

	ARGBEGIN{
	case 'D':		/* for good ps -a listings */
		break;
	case 'f':		/* fall through for backward compatibility */
	case 'g':
	case 'b':
		break;
	default:
		usage();
	}ARGEND

	setprocname(argv0);

	@autoreleasepool{
		[NSApplication sharedApplication];
		[NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
		myApp = [AppDelegate new];
		[NSApp setDelegate:myApp];
		[NSApp run];
	}
}

@implementation AppDelegate

+ (void)callservep9p:(id)arg
{
	servep9p();
	[NSApp terminate:self];
}

+ (void)makewin:(NSValue *)v
{
	NSRect r, sr;
	Rectangle wr;
	int set;
	char *s;

	const NSWindowStyleMask Winstyle = NSWindowStyleMaskTitled
		| NSWindowStyleMaskClosable
		| NSWindowStyleMaskMiniaturizable
		| NSWindowStyleMaskResizable;

	sr = [[NSScreen mainScreen] frame];
	r = [[NSScreen mainScreen] visibleFrame];

	s = [v pointerValue];
	LOG(@"makewin(%s)", s);
	if(s && *s){
		if(parsewinsize(s, &wr, &set) < 0)
			sysfatal("%r");
	}else{
		wr = Rect(0, 0, sr.size.width*2/3, sr.size.height*2/3);
		set = 0;
	}

	r.origin.x = wr.min.x;
	r.origin.y = sr.size.height-wr.max.y;	/* winsize is top-left-based */
	r.size.width = fmin(Dx(wr), r.size.width);
	r.size.height = fmin(Dy(wr), r.size.height);
	r = [NSWindow contentRectForFrameRect:r
		styleMask:Winstyle];

	win = [[NSWindow alloc]
		initWithContentRect:r
		styleMask:Winstyle
		backing:NSBackingStoreBuffered defer:NO];
	[win setTitle:@"devdraw"];

	if(!set)
		[win center];
	[win setCollectionBehavior:
		NSWindowCollectionBehaviorFullScreenPrimary];
	[win setContentMinSize:NSMakeSize(128,128)];

	[win setRestorable:NO];
	[win setAcceptsMouseMovedEvents:YES];
	[win setDelegate:myApp];
	// [win setDisplaysWhenScreenProfileChanges:NO];

	myContent = [DevDrawView new];
	[win setContentView:myContent];
	[myContent.layer setOpaque:YES];
	[myContent setLayerContentsRedrawPolicy:
		NSViewLayerContentsRedrawOnSetNeedsDisplay];

	topwin();
}

+ (void)callkicklabel:(NSString *)s
{
	LOG(@"callkicklabel(%@)", s);
	[win setTitle:s];
	[[NSApp dockTile] setBadgeLabel:s];
}

+ (void)callsetNeedsDisplayInRect:(NSValue *)v
{
	NSRect r;

	r = [v rectValue];
	LOG(@"callsetNeedsDisplayInRect(%g, %g, %g, %g)", r.origin.x, r.origin.y, r.size.width, r.size.height);
	r = [win convertRectFromBacking:r];
	LOG(@"setNeedsDisplayInRect(%g, %g, %g, %g)", r.origin.x, r.origin.y, r.size.width, r.size.height);
	[myContent setNeedsDisplayInRect:r];
}

+ (void)callsetcursor:(NSValue *)v
{
	Cursor *c;
	NSBitmapImageRep *r;
	NSImage *i;
	NSPoint p;
	int b;
	uchar *plane[5];

	qlock(&cursorl);
	c = [v pointerValue];
	if(!c)
		c = &bigarrow;
	r = [[NSBitmapImageRep alloc]
		initWithBitmapDataPlanes:nil
		pixelsWide:16
		pixelsHigh:16
		bitsPerSample:1
		samplesPerPixel:2
		hasAlpha:YES
		isPlanar:YES
		colorSpaceName:NSDeviceWhiteColorSpace
		bytesPerRow:2
		bitsPerPixel:1];

	[r getBitmapDataPlanes:plane];

	for(b=0; b<2*16; b++){
		plane[0][b] = ~c->set[b];
		plane[1][b] = c->clr[b];
	}
	p = NSMakePoint(-c->offset.x, -c->offset.y);
	i = [[NSImage alloc] initWithSize:NSMakeSize(16, 16)];
	[i addRepresentation:r];

	currentCursor = [[NSCursor alloc] initWithImage:i hotSpot:p];
	qunlock(&cursorl);

	[win invalidateCursorRectsForView:myContent];
}

- (void)applicationDidFinishLaunching:(id)arg
{
	NSMenu *m, *sm;
	NSData *d;
	NSImage *i;

	LOG(@"applicationDidFinishLaunching");

	sm = [NSMenu new];
	[sm addItemWithTitle:@"Hide" action:@selector(hide:) keyEquivalent:@"h"];
	[sm addItemWithTitle:@"Quit" action:@selector(terminate:) keyEquivalent:@"q"];
	m = [NSMenu new];
	[m addItemWithTitle:@"DEVDRAW" action:NULL keyEquivalent:@""];
	[m setSubmenu:sm forItem:[m itemWithTitle:@"DEVDRAW"]];
	[NSApp setMainMenu:m];

	d = [[NSData alloc] initWithBytes:glenda_png length:(sizeof glenda_png)];
	i = [[NSImage alloc] initWithData:d];
	[NSApp setApplicationIconImage:i];
	[[NSApp dockTile] display];

	[NSThread
		detachNewThreadSelector:@selector(callservep9p:)
		toTarget:[self class] withObject:nil];
}

- (NSApplicationPresentationOptions)window:(id)arg
		willUseFullScreenPresentationOptions:(NSApplicationPresentationOptions)proposedOptions {
	NSApplicationPresentationOptions o;
	o = proposedOptions;
	o &= ~(NSApplicationPresentationAutoHideDock | NSApplicationPresentationAutoHideMenuBar);
	o |= NSApplicationPresentationHideDock | NSApplicationPresentationHideMenuBar;
	return o;
}

- (void)windowDidEndLiveResize:(id)arg
{
	resizeimg();
}


- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)theApplication {
	return YES;
}

- (void)applicationWillTerminate:(id)arg
{
	if(colorspace)
		CGColorSpaceRelease(colorspace);
	if(imgbuf)
		CGImageRelease(imgbuf);
	if(imgContext)
		CGContextRelease(imgContext);
}

@end

@implementation DevDrawView

- (BOOL)wantsUpdateLayer
{
	return YES;
}

- (void)updateLayer
{
	if(!imgbufReady){
		LOG(@"img not initialized");
		return;
	}
	LOG(@"updateLayer");
	qlock(&imgbufl);
	[self.layer setContents:(__bridge id)imgbuf];
	qunlock(&imgbufl);
}

- (BOOL)isOpaque
{
	return YES;
}

- (BOOL)isFlipped
{
	return YES;	/* to make the content's origin top left */
}

- (BOOL)acceptsFirstResponder
{
	return YES;	/* else no keyboard */
}

- (void)mouseMoved:(NSEvent*)e{ [self getmouse:e];}
- (void)mouseDown:(NSEvent*)e{ [self getmouse:e];}
- (void)mouseDragged:(NSEvent*)e{ [self getmouse:e];}
- (void)mouseUp:(NSEvent*)e{ [self getmouse:e];}
- (void)otherMouseDown:(NSEvent*)e{ [self getmouse:e];}
- (void)otherMouseDragged:(NSEvent*)e{ [self getmouse:e];}
- (void)otherMouseUp:(NSEvent*)e{ [self getmouse:e];}
- (void)rightMouseDown:(NSEvent*)e{ [self getmouse:e];}
- (void)rightMouseDragged:(NSEvent*)e{ [self getmouse:e];}
- (void)rightMouseUp:(NSEvent*)e{ [self getmouse:e];}

- (void)scrollWheel:(NSEvent*)e
{
	NSInteger s;

	s = [e scrollingDeltaY];
	if(s > 0)
		[self sendmouse:8];
	else if (s < 0)
		[self sendmouse:16];
}

- (void)keyDown:(NSEvent*)e{ [self getkeyboard:e];}
- (void)flagsChanged:(NSEvent*)e{ [self getkeyboard:e];}

- (void)magnifyWithEvent:(NSEvent*)e
{
	if(fabs([e magnification]) > 0.02)
		[[self window] toggleFullScreen:nil];
}

- (void)getkeyboard:(NSEvent *)e
{
	static NSEventModifierFlags omod;
	NSEventModifierFlags m;
	uint k, b;

	m = [e modifierFlags];

	switch([e type]){
	case NSKeyDown:
		k = keycvt(e);

		if(m & NSEventModifierFlagCommand){
			if((m & NSEventModifierFlagShift) && 'a' <= k && k <= 'z')
				k += 'A' - 'a';
			if(' '<=k && k<='~')
				keystroke(Kcmd+k);
			break;
		}
		if(k>0)
			keystroke(k);
		break;

	case NSFlagsChanged:
		b = [NSEvent pressedMouseButtons];
		b = (b&~6) | (b&4)>>1 | (b&2)<<1;
		if(b){
			if(m & ~omod & NSEventModifierFlagControl)
				b |= 1;
			if(m & ~omod & NSEventModifierFlagOption)
				b |= 2;
			if(m & ~omod & NSEventModifierFlagCommand)
				b |= 4;
			[self sendmouse:b];
		}else
		if(m & ~omod & NSEventModifierFlagOption)
			keystroke(Kalt);
		break;

	default:
		panic("getkey: unexpected event type");
	}
	omod = m;
}

- (void)getmouse:(NSEvent *)e
{
	NSUInteger b;
	NSEventModifierFlags m;

	b = [NSEvent pressedMouseButtons];
	b = b&~6 | (b&4)>>1 | (b&2)<<1;
	b = mouseswap(b);

	if(b == 1){
		m = [e modifierFlags];
		if(m & NSEventModifierFlagOption){
			abortcompose();
			b = 2;
		}else
		if(m & NSEventModifierFlagCommand)
			b = 4;
	}
	[self sendmouse:b];
}

- (void)sendmouse:(NSUInteger)b
{
	NSPoint p;

	p = [self.window convertPointToBacking:
		[self.window mouseLocationOutsideOfEventStream]];
	p.y = Dy(mouserect) - p.y;
	// LOG(@"(%g, %g) <- sendmouse(%d)", p.x, p.y, (uint)b);
	mousetrack(p.x, p.y, b, msec());
}

- (void)resetCursorRects {
	[super resetCursorRects];
	[self addCursorRect:self.bounds cursor:currentCursor];
}

@end

static uint
msec(void)
{
	return nsec()/1000000;
}

static uint
keycvt(NSEvent *event)
{
	NSString *s = [event characters];
	int code = [s characterAtIndex:0];
	switch(code){
	case '\r': return '\n';
	case '\b': return 127;
	case 127: return '\b';
	case NSUpArrowFunctionKey: return Kup;
	case NSDownArrowFunctionKey: return Kdown;
	case NSLeftArrowFunctionKey: return Kleft;
	case NSRightArrowFunctionKey: return Kright;
	case NSInsertFunctionKey: return Kins;
	case NSDeleteFunctionKey: return Kdel;
	case NSHomeFunctionKey: return Khome;
	case NSEndFunctionKey: return Kend;
	case NSPageUpFunctionKey: return Kpgup;
	case NSPageDownFunctionKey: return Kpgdown;
	case NSF1FunctionKey: return KF|1;
	case NSF2FunctionKey: return KF|2;
	case NSF3FunctionKey: return KF|3;
	case NSF4FunctionKey: return KF|4;
	case NSF5FunctionKey: return KF|5;
	case NSF6FunctionKey: return KF|6;
	case NSF7FunctionKey: return KF|7;
	case NSF8FunctionKey: return KF|8;
	case NSF9FunctionKey: return KF|9;
	case NSF10FunctionKey: return KF|10;
	case NSF11FunctionKey: return KF|11;
	case NSF12FunctionKey: return KF|12;
	case NSBeginFunctionKey:
	case NSPrintScreenFunctionKey:
	case NSScrollLockFunctionKey:
	case NSF13FunctionKey:
	case NSF14FunctionKey:
	case NSF15FunctionKey:
	case NSF16FunctionKey:
	case NSF17FunctionKey:
	case NSF18FunctionKey:
	case NSF19FunctionKey:
	case NSF20FunctionKey:
	case NSF21FunctionKey:
	case NSF22FunctionKey:
	case NSF23FunctionKey:
	case NSF24FunctionKey:
	case NSF25FunctionKey:
	case NSF26FunctionKey:
	case NSF27FunctionKey:
	case NSF28FunctionKey:
	case NSF29FunctionKey:
	case NSF30FunctionKey:
	case NSF31FunctionKey:
	case NSF32FunctionKey:
	case NSF33FunctionKey:
	case NSF34FunctionKey:
	case NSF35FunctionKey:
	case NSPauseFunctionKey:
	case NSSysReqFunctionKey:
	case NSBreakFunctionKey:
	case NSResetFunctionKey:
	case NSStopFunctionKey:
	case NSMenuFunctionKey:
	case NSUserFunctionKey:
	case NSSystemFunctionKey:
	case NSPrintFunctionKey:
	case NSClearLineFunctionKey:
	case NSClearDisplayFunctionKey:
	case NSInsertLineFunctionKey:
	case NSDeleteLineFunctionKey:
	case NSInsertCharFunctionKey:
	case NSDeleteCharFunctionKey:
	case NSPrevFunctionKey:
	case NSNextFunctionKey:
	case NSSelectFunctionKey:
	case NSExecuteFunctionKey:
	case NSUndoFunctionKey:
	case NSRedoFunctionKey:
	case NSFindFunctionKey:
	case NSHelpFunctionKey:
	case NSModeSwitchFunctionKey: return 0;
	default: return code;
	}
}

Memimage*
attachscreen(char *label, char *winsize)
{
	LOG(@"attachscreen(%s, %s)", label, winsize);
	[AppDelegate
		performSelectorOnMainThread:@selector(makewin:)
		withObject:[NSValue valueWithPointer:winsize]
		waitUntilDone:YES];
	kicklabel(label);
	setcursor(nil);
	return initimg();
}

static Memimage*
initimg(void)
{
	NSSize size;

	size = [myContent convertSizeToBacking:[myContent bounds].size];
	mouserect = Rect(0, 0, size.width, size.height);

	LOG(@"initimg %.0f %.0f", size.width, size.height);

	img = allocmemimage(mouserect, XBGR32);
	if(img == nil)
		panic("allocmemimage: %r");
	if(img->data == nil)
		panic("img->data == nil");

	if(!colorspace)
		colorspace = CGColorSpaceCreateDeviceRGB();

	qlock(&imgbufl);
	if(imgbuf){
		CGImageRelease(imgbuf);
		imgbuf = NULL;
	}
	if(imgContext){
		CGContextRelease(imgContext);
		imgContext = NULL;
	}
	imgContext = CGBitmapContextCreate(NULL, size.width, size.height, 8, 0,
		colorspace, kCGBitmapByteOrder32Big | kCGImageAlphaNoneSkipLast);
	//CGContextTranslateCTM(imgContext, 0, size.height);
	//CGContextScaleCTM(imgContext, 1.0, -1.0);
	imgbufReady = 0;
	qunlock(&imgbufl);

	displaydpi = [win backingScaleFactor] * 110;

	return img;
}

void
_flushmemscreen(Rectangle r)
{
	NSRect sr, dr;
	CGImageRef i;
	CGDataProviderRef provider;
	size_t l;

	LOG(@"_flushmemscreen(%d,%d,%d,%d)", r.min.x, r.min.y, Dx(r), Dy(r));
	dr = NSMakeRect(r.min.x, r.min.y, Dx(r), Dy(r));
       sr = dr;
       LOG(@"sr: %g,%g,%g,%g", sr.origin.x, sr.origin.y, sr.size.width, sr.size.height);
       dr.origin.y = Dy(img->r) - dr.origin.y - dr.size.height;
       LOG(@"dr: %g,%g,%g,%g", dr.origin.x, dr.origin.y, dr.size.width, dr.size.height);

	l = img->width*sizeof(u32int);
	provider = CGDataProviderCreateWithData(NULL,
		byteaddr(img, Pt(sr.origin.x, sr.origin.y)), sr.size.height*l, NULL);
	i = CGImageCreate(sr.size.width, sr.size.height, 8, 32, l,
		colorspace, kCGBitmapByteOrder32Big | kCGImageAlphaNoneSkipLast,
		provider, NULL, false, kCGRenderingIntentDefault);
	CGDataProviderRelease(provider);

	qlock(&imgbufl);
	if(imgbuf){
		CGImageRelease(imgbuf);
		imgbuf = NULL;
	}
	CGContextDrawImage(imgContext, dr, i);
	imgbuf = CGBitmapContextCreateImage(imgContext);
	imgbufReady = 1;
	qunlock(&imgbufl);

	CGImageRelease(i);

	[AppDelegate
		performSelectorOnMainThread:@selector(callsetNeedsDisplayInRect:)
		withObject:[NSValue valueWithRect:NSMakeRect(r.min.x, r.min.y, Dx(r), Dy(r))]
		waitUntilDone:NO];
}

void
setmouse(Point p)
{
	NSPoint q;
	NSInteger h;

	LOG(@"setmouse(%d,%d)", p.x, p.y);
	q = [win convertPointFromBacking:NSMakePoint(p.x, p.y)];
	LOG(@"(%g, %g) <- fromBacking", q.x, q.y);
	q = [myContent convertPoint:q toView:nil];
	LOG(@"(%g, %g) <- toWindow", q.x, q.y);
	q = [win convertPointToScreen:q];
	h = [[NSScreen mainScreen] frame].size.height;
	q.y = h - q.y;
	LOG(@"(%g, %g) <- setmouse", q.x, q.y);
	CGWarpMouseCursorPosition(NSPointToCGPoint(q));
	CGAssociateMouseAndMouseCursorPosition(true);
}

char*
getsnarf(void)
{
	NSPasteboard *pb;
	NSString *s;

	@autoreleasepool{
		pb = [NSPasteboard generalPasteboard];

		qlock(&snarfl);
		s = [pb stringForType:NSPasteboardTypeString];
		qunlock(&snarfl);

		if(s)
			return strdup((char *)[s UTF8String]);
		else
			return nil;
	}
}

void
putsnarf(char *s)
{
	NSArray *t;
	NSPasteboard *pb;
	NSString *str;

	if(strlen(s) >= SnarfSize)
		return;

	@autoreleasepool{
		t = [NSArray arrayWithObject:NSPasteboardTypeString];
		pb = [NSPasteboard generalPasteboard];
		str = [[NSString alloc] initWithUTF8String:s];

		qlock(&snarfl);
		[pb declareTypes:t owner:nil];
		[pb setString:str forType:NSPasteboardTypeString];
		qunlock(&snarfl);
	}
}

void
kicklabel(char *label)
{
	NSString *s;

	LOG(@"kicklabel(%s)", label);
	if(label == nil)
		return;

	@autoreleasepool{
		s = [[NSString alloc] initWithUTF8String:label];
		[AppDelegate
			performSelectorOnMainThread:@selector(callkicklabel:)
			withObject:s
			waitUntilDone:NO];
	}
}

void
setcursor(Cursor *c)
{
	[AppDelegate
		performSelectorOnMainThread:@selector(callsetcursor:)
		withObject:[NSValue valueWithPointer:c]
		waitUntilDone:YES];
}

void
topwin(void)
{
	[win
		performSelectorOnMainThread:
		@selector(makeKeyAndOrderFront:)
		withObject:nil
		waitUntilDone:YES];

	[NSApp activateIgnoringOtherApps:YES];
}

void
resizeimg(void)
{
	_drawreplacescreenimage(initimg());

	mouseresized = 1;
}

static void
setprocname(const char *s)
{
  CFStringRef process_name;
  
  process_name = CFStringCreateWithBytes(nil, (uchar*)s, strlen(s), kCFStringEncodingUTF8, false);

  // Adapted from Chrome's mac_util.mm.
  // http://src.chromium.org/viewvc/chrome/trunk/src/base/mac/mac_util.mm
  //
  // Copyright (c) 2012 The Chromium Authors. All rights reserved.
  //
  // Redistribution and use in source and binary forms, with or without
  // modification, are permitted provided that the following conditions are
  // met:
  //
  //    * Redistributions of source code must retain the above copyright
  // notice, this list of conditions and the following disclaimer.
  //    * Redistributions in binary form must reproduce the above
  // copyright notice, this list of conditions and the following disclaimer
  // in the documentation and/or other materials provided with the
  // distribution.
  //    * Neither the name of Google Inc. nor the names of its
  // contributors may be used to endorse or promote products derived from
  // this software without specific prior written permission.
  //
  // THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  // "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  // LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  // A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  // OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  // SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  // LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  // DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  // THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  // (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  // OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  // Warning: here be dragons! This is SPI reverse-engineered from WebKit's
  // plugin host, and could break at any time (although realistically it's only
  // likely to break in a new major release).
  // When 10.7 is available, check that this still works, and update this
  // comment for 10.8.

  // Private CFType used in these LaunchServices calls.
  typedef CFTypeRef PrivateLSASN;
  typedef PrivateLSASN (*LSGetCurrentApplicationASNType)();
  typedef OSStatus (*LSSetApplicationInformationItemType)(int, PrivateLSASN,
                                                          CFStringRef,
                                                          CFStringRef,
                                                          CFDictionaryRef*);

  static LSGetCurrentApplicationASNType ls_get_current_application_asn_func =
      NULL;
  static LSSetApplicationInformationItemType
      ls_set_application_information_item_func = NULL;
  static CFStringRef ls_display_name_key = NULL;

  static bool did_symbol_lookup = false;
  if (!did_symbol_lookup) {
    did_symbol_lookup = true;
    CFBundleRef launch_services_bundle =
        CFBundleGetBundleWithIdentifier(CFSTR("com.apple.LaunchServices"));
    if (!launch_services_bundle) {
      fprint(2, "Failed to look up LaunchServices bundle\n");
      return;
    }

    ls_get_current_application_asn_func =
        (LSGetCurrentApplicationASNType)(
            CFBundleGetFunctionPointerForName(
                launch_services_bundle, CFSTR("_LSGetCurrentApplicationASN")));
    if (!ls_get_current_application_asn_func)
      fprint(2, "Could not find _LSGetCurrentApplicationASN\n");

    ls_set_application_information_item_func =
        (LSSetApplicationInformationItemType)(
            CFBundleGetFunctionPointerForName(
                launch_services_bundle,
                CFSTR("_LSSetApplicationInformationItem")));
    if (!ls_set_application_information_item_func)
      fprint(2, "Could not find _LSSetApplicationInformationItem\n");

    CFStringRef* key_pointer = (CFStringRef*)(
        CFBundleGetDataPointerForName(launch_services_bundle,
                                      CFSTR("_kLSDisplayNameKey")));
    ls_display_name_key = key_pointer ? *key_pointer : NULL;
    if (!ls_display_name_key)
      fprint(2, "Could not find _kLSDisplayNameKey\n");

    // Internally, this call relies on the Mach ports that are started up by the
    // Carbon Process Manager.  In debug builds this usually happens due to how
    // the logging layers are started up; but in release, it isn't started in as
    // much of a defined order.  So if the symbols had to be loaded, go ahead
    // and force a call to make sure the manager has been initialized and hence
    // the ports are opened.
    ProcessSerialNumber psn;
    GetCurrentProcess(&psn);
  }
  if (!ls_get_current_application_asn_func ||
      !ls_set_application_information_item_func ||
      !ls_display_name_key) {
    return;
  }

  PrivateLSASN asn = ls_get_current_application_asn_func();
  // Constant used by WebKit; what exactly it means is unknown.
  const int magic_session_constant = -2;
  OSErr err =
      ls_set_application_information_item_func(magic_session_constant, asn,
                                               ls_display_name_key,
                                               process_name,
                                               NULL /* optional out param */);
  if(err != noErr)
    fprint(2, "Call to set process name failed\n");
}
