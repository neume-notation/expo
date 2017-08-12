Medieval Fragments: Old Hispanic chant vestiges
===============================================

This is an outreach project, designed to present to the public some of
the research on medieval chant, especially that from the Iberian Peninsular.
It consists of a computer-based exhibition, which was designed to be displayed
alongside the medieval manuscripts to which it refers.  It is also
available [online](http://expo.neumes.org.uk/).  Four variants of
the content are provided, corresponding to the four manuscripts held in
museums and libraries in Spain and Portugal where the exhibition is installed.
The user interface and content are provided in English, Spanish and
Portuguese, and can be switched between languages at any point.  Therefore
many things are provided in a total of twelve versions - four manuscript
variants times three languages - which accounts for much of the complexity of
the project.

The project is an example of using Purescript, Halogen and Yesod together,
and is made open-source in the hope that it contains some useful ideas.
Program code and the majority of the content are available here,
but images and videos are mostly dummies, for the reasons given below.
The code could definitely benefit from some cleaning up, but it is commented,
and should be reasonably clear as it stands.

Code Description
----------------

The main component is the user interface, written in
[Purescript](http://www.purescript.org/) and
[Halogen](https://github.com/slamdata/purescript-halogen).
It is designed primarily for use in a public place with a 23" touch screen,
and no keyboard.

The entire interface is a single page application, with a single entry point
into the Purescript code.  Once loaded, it has all
of the data it needs, apart from static images and videos.  Therefore
everything can be delivered from filestore, and no server is required.  (There
is an AJAX call for logging - see below - but it is not essential to have a
server listening for it since it is OK for it to fail.)

For use at the exhibition sites, the HTML page is loaded and run using a
simple [Electron](https://github.com/electron/electron) application.  This
application sets full-screen kiosk mode, and has code to prevent zooming.
It also provides a trivial HTTP server used for recording very compact
logging information, which can be used to create estimates of the numbers
of visitors, and the extent to which they explored beyond the introductory
page.  The HTML used in this situation contains javascript code to return to
the start page after a timeout, and to suppress unnecessary and potentially
confusing touch-screen gestures, such as long touch.

With only very small changes, the same HTML page is used for an online version
of the exhibition.  Also cut-down static pages are created for mobiles and
tablets, since small devices are unlikely to be able to zoom out sufficiently
to accomodate the full user interface.

The data required to drive the exhibition is all determined at build time,
but it is relatively complex: there are tables of timings within the videos,
used to control the display of illustrations and the scrolling of the
transcripts; texts of transcripts and short messages are provided in three
languages; and a few hundred static URLs point to images and videos.
In order to keep these under control, and to check that the static
resources are actually present at build time, the data structure is created
as a JSON object using a simple web server written using
[Yesod](https://www.yesodweb.com/).  This server is used only during the build,
and the page it creates is simply captured for stand-alone use (after minor
post-processing).

Images and Videos in this Repository
------------------------------------

Many of the images included in the live product are used by agreement with
the copyright holders, and we are not free to distribute them separately.
The images concerned have been processed to blank out the copyrighted
material, but they have the same size as the live versions.  Therefore the
code can be built and run, but will display fairly meaningless content.
The full videos are also omitted because they are large, and instead the
names are linked to a single test video.

You may find it helpful to view the online version, so that
you can see how the real videos and images interrelate.  You can find it at
[expo.neumes.org.uk](http://expo.neumes.org.uk/) - please visit it using
a full-size screen, so that you are not diverted to the cut-down mobile
version.

Directory Layout
----------------

The top-level directories are used as follows

> **Build**  
> Contains major components which are built more or less independently.
> These include the Purescript user interface, CSS, and the Windows installer
> generator.  Build tools are under `Build/Scripts`.
>
> **static**  
> The static hierarchy, containing images, videos, javascript etc.  In the
> final product it includes the built Purescript and the merged CSS, but
> in the development hierarchy these are symbolic links.
>
> **CEAPIntegration and templates**  
> These contain pieces of the Yesod server used at build time, the rest being
> constructed on the fly from a `stack` template (see
> `Build/Scripts/makeSinglePage.sh`).  The Yesod build also uses
> `static` directly.
>
> Templates and files in `templates/data` define the exhibition content, and
> are combined into a single javascript object `expoData` which is used by
> the Purescript code.  The file `messages.xml` is designed to be converted
> to and from Excel to be sent to human translators - see the comment in
> `Build/Scripts/xmlmessages.py`.
>
> **StandAlone**  
> The Electron application - the `index.html` referenced by `run.js`
> is the file produced by the Yesod server at build time and placed in
> this directory by `Build/Scripts/buildall.sh`.
> The `static` hierarchy is logically included in `StandAlone`, but in the
> development hierarchy symbolic links are used.
>
> **WebSite**  
> Contains the structure of the static web site for the online version,
> and, similarly to `StandAlone`, the `index.html` and mobile-friendly HTML
> are copied in by the build tools.

Symbolic links are used in several places, and most of them need to be
dereferenced when constructing the installer or copying the static web site
to a server.  The exceptions are the two in `WebSite/small`, which can
optionally be left as links.

Requirements
------------

The build tools provided here run on Linux.  It should be possible to set up
equivalent scripts on Windows or OSX, but you are on your own!

The following tools need to be installed globally, or at least globally
within your user filestore (e.g. by setting a `prefix` in your `.npmrc`).

  1. `stack` - [https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/)
  2. `ghc` - [https://www.haskell.org/ghc/](https://www.haskell.org/ghc/) or [https://launchpad.net/~hvr/+archive/ubuntu/ghc](https://launchpad.net/~hvr/+archive/ubuntu/ghc)
  3. `node` and `npm` - [https://docs.npmjs.com/getting-started/installing-node](https://docs.npmjs.com/getting-started/installing-node)
  4. `purescript` compiler - [http://www.purescript.org/](http://www.purescript.org/)
  5. npm packages: `pulp`, `bower`, `minifier`
  6. `cleancss` command (e.g. the Ubuntu "cleancss" package)
  7. `electron` if you want to run the stand-alone version - [https://github.com/electron/electron](https://github.com/electron/electron)

With those prerequisites, it should be possible to run `Build/Scripts/buildall`
and have the results built in `StandAlone` and `WebSite`.  If you
want to copy one of those resulting hierarchies, please note the remark
above about the use of symbolic links.

Issues and Pull Requests
------------------------

Comments, documentation and code cleanup are welcome, with the aim of
making this code a clear and useful example.
