{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.About
  ( getAboutR
  ) where

import Import
import Paths_qua_server_ura (version)
import Data.Version (showVersion)

getAboutR :: Handler Html
getAboutR =
    fullLayout Nothing "About us" $ do
      setTitle "About qua-kit"
      toWidgetBody $
        [hamlet|
          <div class="row">

            <div class="col-lg-6 col-md-5 col-sm-8 col-xs-9">
              <div.card.card-orange>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5.margin-bottom-no.margin-top-no>
                        Chair of Information Architecture
                  <div.card-inner>
                    <p>
                      The project is developed within our chair at ETH Zurich.
                    <blockquote>
                      “We develop visual methods for the analysis, design and simulation #
                      of urban systems for a sustainable future”
                  <div.card-action>
                    <div.card-action-btn.pull-left>
                      <a.btn.btn-flat.waves-attach.waves-light.waves-effect href="http://www.ia.arch.ethz.ch/" target="_blank">
                        <span.icon>check
                        &nbsp;Visit our site


            <div class="col-lg-6 col-md-5 col-sm-8 col-xs-9">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5.margin-bottom-no.margin-top-no>
                        Qua-kit v#{showVersion version}
                  <div.card-inner>
                    <p>
                      Qua-kit is a web platform for viewing and manipulating simple urban geometry. #
                    <p>
                      Logged in via edX platform you can work on a single design problem, #
                      share your ideas together with design proposals, view and discuss works of others.
                    <p>
                      Local clients can use qua-kit as a presentation tool.
                      Connected to a computational server backend #
                      it can use registered remote services for visual analysis of geometry.
                    <p>
                      The platform is an open source project; #
                      therefore, everyone is welcome to contribute.
                  <div.card-action>
                    <div.card-action-btn.pull-left>
                      <a.btn.btn-flat.waves-attach.waves-light.waves-effect href="https://github.com/achirkin/qua-kit/" target="_blank">
                        <span.icon>check
                        &nbsp;Visit project's page



            <div class="col-lg-6 col-md-5 col-sm-8 col-xs-9">
              <div.card.card-brand>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5.margin-bottom-no.margin-top-no>
                        ADvISE
                  <div.card-inner>
                    <p>
                      One of this platform's goals is to gather data for ADvISE research project.
                    <p>
                      ADvISE stands for #
                       Data Analysis for understanding the Impact of urban design on Social pErformance of a city.
                    <p>
                      We aim to produce an open framework with basic functionality #
                      to efficiently search for compromise solutions for complex planning problems #
                      and an experimental software prototype #
                      with an intuitive user interface for representing planning problems #
                      and presenting optimal solutions at various stages of the design process.
                  <div.card-action>
                    <div.card-action-btn.pull-left>
                      <a.btn.btn-flat.waves-attach.waves-light.waves-effect href="http://www.ia.arch.ethz.ch/advise/" target="_blank">
                        <span.icon>check
                        &nbsp;Visit project's page



            <div class="col-lg-6 col-md-5 col-sm-8 col-xs-9">
              <div.card.card-red>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5.margin-bottom-no.margin-top-no>
                        Empower Shack by U-TT ETH Zurich
                  <div.card-inner>
                    <p>
                      Our first case study for the online course on edX platform is provided by Urban-ThinkTank group at ETH Zurich.
                    <p>
                      Empower Shack is an interdisciplinary development project directed by U-TT, #
                      ETH Zurich and the local NGO Ikhayalami Development Services, in collaboration with the BT-Section #
                      community and associated local and international partners.
                    <p>
                      The ongoing pilot phase is focused on a cluster of 68 houses within the BT-Section of Khayelitsha. #
                      Through innovative design and organisational models, the project aims to develop a comprehensive and #
                      sustainable informal settlement upgrading strategy centred on four core components: a two-story housing prototype, #
                      participatory spatial planning, ecological landscape management, and integrated livelihoods programming.
                  <div.card-action>
                    <div.card-action-btn.pull-left>
                      <a.btn.btn-flat.waves-attach.waves-light.waves-effect href="http://u-tt.com/project/empower-shack/" target="_blank">
                        <span.icon>check
                        &nbsp;Visit Empower Shack page
        |]
