-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.About
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Handler.About
  ( getAboutR
  ) where

import Import

getAboutR :: Handler Html
getAboutR =
    fullLayout Nothing "About us" $ do
      setTitle "About qua-kit"
      toWidgetBody $
        [hamlet|
          <div class="row">


            <div class="col-lg-6 col-md-5 col-sm-8 col-xs-9">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5.margin-bottom-no.margin-top-no>
                        Qua-kit
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

        |]

