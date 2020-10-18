module Page.Resources exposing (..)

import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, type_)
import Html.Events exposing (onClick)


type Msg
    = NoOp


view : Html Msg
view =
    article [ class "resources" ]
        [ section [ class "resources__section" ]
            -- </SALT>
            [ span [ class "resources__list-heading" ] [ text "</salt> originals" ]
            , ul [ class "resources__list" ]
                [ li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://appliedtechnology.github.io/protips/" ] [ text "Pro Tips" ] ]

                -- , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "URLURL" ] [ text "LINK" ] ]
                ]
            ]
        , section [ class "resources__section" ]
            --  JAVASCRIPT
            [ span [ class "resources__list-heading" ] [ text "javascript" ]
            , ul [ class "resources__list" ]
                [ li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://developer.mozilla.org/en-US/docs/Web/JavaScript" ] [ text "Mozilla Developer Network" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://expressjs.com/" ] [ text "express.js" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://guide.elm-lang.org/" ] [ text "bla bla" ] ]
                ]
            ]
        , section [ class "resources__section" ]
            --   PRIVACY
            [ span [ class "resources__list-heading" ] [ text "privacy" ]
            , ul [ class "resources__list" ]
                [ li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://passwordsgenerator.net/?length=32&symbols=1&numbers=1&lowercase=1&uppercase=1&similar=1&ambiguous=1&client=1&autoselect=1" ] [ text "PasswordsGenerator" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://tails.boum.org/" ] [ text "Tails: portable OS" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://www.torproject.org/" ] [ text "TOR onion browser (\"dark web\")" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://wiki.gnupg.org/WKD" ] [ text "GnuPG" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://ssd.eff.org/" ] [ text "Surveillance Self-Defense" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://www.eff.org/" ] [ text "Electronic Frontier Foundation" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://github.com/sbilly/awesome-security" ] [ text "Awesome Security (on GitHub)" ] ]
                ]
            ]
        , section [ class "resources__section" ]
            --   ELM
            [ span [ class "resources__list-heading" ] [ text "elm" ]
            , ul [ class "resources__list" ]
                [ li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://package.elm-lang.org/" ] [ text "packages" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://package.elm-lang.org/packages/truqu/elm-oauth2/latest" ] [ text "Elm OAuth 2" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://mbylstra.github.io/html-to-elm/" ] [ text "HTML to Elm" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://guide.elm-lang.org/" ] [ text "official guide" ] ]
                ]
            ]
        , section [ class "resources__section" ]
            --   OTHERS
            [ span [ class "resources__list-heading" ] [ text "others" ]
            , ul [ class "resources__list" ]
                [ li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://archive.parrotsec.org/parrot/misc/openbooks/" ] [ text "Open Books (hosted at ParrotSec)" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://www.exploit-db.com/" ] [ text "Exploit Database" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://owasp.org/" ] [ text "The Open Web Application Security Project" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://www.thoughtworks.com/radar" ] [ text "Technology Radar (thoughtworks)" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://spritepad.wearekiss.com/" ] [ text "spritepad" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://purecss.io/" ] [ text "pure.css" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://fontawesome.com/" ] [ text "fontawesome" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://tinypng.com/" ] [ text "TinyPNG" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://cssminifier.com/" ] [ text "css-minifier" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "http://minifycode.com/html-minifier/" ] [ text "Html minifier(code-minifier)" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://dnschecker.org/" ] [ text "DNS-checker" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://www.awwwards.com/" ] [ text "awwwards.com" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "http://microformats.org/" ] [ text "microformats (SEO)" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://cssguidelin.es/" ] [ text "css guidelines - high level advice (2017)" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://caniuse.com/" ] [ text "can i use.. ?" ] ]
                ]
            ]
        , section [ class "resources__section" ]
            --   FREE STOCK RESOURCES
            [ span [ class "resources__list-heading" ] [ text "Free Stock Resources" ]
            , ul [ class "resources__list" ]
                [ li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://www.storyblocks.com/" ] [ text "story blocks (video, audio, photos)" ] ]
                , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://www.pexels.com/" ] [ text "pexels (photo, video)" ] ]
                ]
            ]
        , section [ class "resources__section" ]
            [ span [ class "resources__list-heading" ] [ text "Typography" ]
            , ul [ class "resources__list" ]
                [ ul [ class "resources__list" ]
                    [ span [ class "resources__list-heading resources__list-subheading" ] [ text "use" ]
                    , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://fonts.google.com/" ] [ text "Google Fonts" ] ]
                    , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://www.fontspring.com/" ] [ text "FontSpring" ] ]
                    , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://www.fontsquirrel.com/" ] [ text "FontSquirrel" ] ]
                    ]
                , ul [ class "resources__list" ]
                    [ span [ class "resources__list-heading resources__list-subheading" ] [ text "read" ]
                    , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://design.tutsplus.com/articles/a-20-minute-intro-to-typography-basics--psd-3326" ] [ text "20min basics" ] ]
                    , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://www.smashingmagazine.com/2011/03/how-to-choose-a-typeface/" ] [ text "choosing a font (smashing magazin)" ] ]
                    , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "https://fontsinuse.com/" ] [ text "Real World Examples" ] ]
                    ]
                ]
            ]

        -- TEMPLATE SECTION
        --   ,  section [ class "resources__section" ]
        --       [ span [ class "resources__list-heading" ] [ text "HEADING" ]
        --       , ul [ class "resources__list" ]
        --           [ li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "URLURL" ] [ text "LINK" ] ]
        --           , li [ class "resources__list-item" ] [ a [ class "resources__list-link", href "URLURL" ] [ text "LINK" ] ]
        --           ]
        --       ]https://appliedtechnology.github.io/protips/
        ]
