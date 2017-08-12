This is the web version of the exhibition.  It is a static site, which does
not depend on any server-side software.

It does, however, need to be set up properly in the server:

  - The Apache virtual host should be set up to respect the type-map file
    `/small/index.html.var`, which is used to negotiate the language for
    the mobile index page.  Suitable fallback and priority should be
    provided in case of no match or multiple matches.  Therefore it needs
    directives:

        AddHandler type-map .var
        ForceLanguagePriority Prefer Fallback
        LanguagePriority en es pt

  - The top-level URL `/` should redirect to `/small/index.html.var`, so that
    mobile users do not have to download large files before they are
    redirected back to the small site, and so that the language is negotiated.

  - There is no `index.html` in `/small/`, so either rewrite rules should be
    provided, or a link `index.html` pointing to `en_index.html`, in case
    someone tries to navigate directly to `/small/`

  - A link `small/static` should be made to `full/static`

  - Videos need to be linked or copied in to the static hierarchy under
    `full/`
