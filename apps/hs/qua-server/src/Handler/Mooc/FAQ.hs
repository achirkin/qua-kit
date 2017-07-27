{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.FAQ
  ( getFAQR
  ) where

import Import

getFAQR :: Handler Html
getFAQR =
    fullLayout Nothing "Frequently Asked Questions" $ do
      setTitle "Qua-kit: FAQ"
      toWidgetBody $
        [hamlet|
          <div class="row">

            <div class="col-lg-8 col-md-9 col-sm-10 col-xs-12">
              <div.card.card-red>
                <div.card-main>
                  <div.card-inner>
                    In September-November 2016 we had the first run of our online course with qua-kit.
                    At that time we received a number of questions from students regarding the work of the system.
                    On this page you can find most common of them together with our answers.
                    Please, try to find an answer to your question here before asking it by mail or via edX platform.

            <div class="col-lg-8 col-md-9 col-sm-10 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-inner>
                    <ol id="outlineList">

            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5 style="padding: 80px 0px 0px 0px; margin: -80px 0px 0px 0px;" #question-1>
                        I want other edX students to see and comment my design proposal!
                  <div.card-inner>
                    <p>
                      This is hard to force unless they want to, right?
                    <p>
                      As the first step, we would suggest to de-anonymize yourself
                      by changing your login name in qua-kit (upper right corner of the site pages).
                      If you put the same nickname at qua-kit as you have at edX,
                      it is easier for others to find you.
                    <p>
                      Second, try to write an interesting but concise description
                      of your proposal when submitting/updating a design.
                      Third, you can copy a direct link to your design submission
                      and publish it in edX discussion (or somewhere else?).

            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5 style="padding: 80px 0px 0px 0px; margin: -80px 0px 0px 0px;" #question-2>
                        I cannot move/rotate building blocks!
                  <div.card-inner>
                    <p>
                      A. Make sure you get familiar with the controls.
                      First, hold primary mouse button for panning, hold secondary mouse button (or primary + shift, or primary + ctrl) for rotating.
                      These rules work for camera and for buildings. To move/rotate a block you first need to click on it (so it becomes red) to activate,
                      and then drag using mouse to move/rotate.
                    <p>
                      B. Something may be wrong with your browser.
                      Make sure you use the latest version of chrome/firefox/safari (chrome is reported to work best).
                      If this does not work, we kindly ask you to submit an issue, so we could resolve it as soon as possible
                      (
                      <a href="@{FeedbackR}">@{FeedbackR}
                      ).
                      When you report an issue, please specify following:
                      <ul>
                        <li>The browser you use (it will be very helpful if you can find out the version too).
                        <li>The Operating System (Linux/Windows/Mac?)
                        <li>Are you using mouse or touchpad?
                        <li>Are you able to only move or only rotate buildings, or nothing at all? Can you select a building by clicking on it (so it turns red)?


            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5 style="padding: 80px 0px 0px 0px; margin: -80px 0px 0px 0px;" #question-3>
                        I cannot use touchpad!
                  <div.card-inner>
                    <p>
                      Due to the different nature of touchpads on different operating
                      systems and browsers, it is difficult to make a touchpad behavior persistent across
                      all of them. Touchpad may work to some extent, but we strongly advise you to use
                      mouse controls.
                    <p>
                      On the other hand, if your screen supports multitouch, you
                      can move camera and blocks using it - in a much more convenient manner!

            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5 style="padding: 80px 0px 0px 0px; margin: -80px 0px 0px 0px;" #question-4>
                        I cannot see anything!
                  <div.card-inner>
                    <p>
                      Something may be wrong with your browser.
                    <p>
                      Make sure you use the latest version of chrome/firefox/safari
                      (chrome is reported to work best).
                    <p>
                      If this does not work, we kindly ask you to submit an issue,
                      so we could resolve it as soon as possible
                      (
                      <a href="@{FeedbackR}">@{FeedbackR}
                      ).
                    <p>
                      When you report an issue, please specify following:
                    <ul>
                      <li> Which link do you use to start the exercise? Note, at least
                        for a first time, you must use “Go!” button on the exercise 1 page.
                      <li> The browser you use (it will be very helpful if you can
                        find out the version too).
                      <li> The Operating System (Linux/Windows/Mac?)


            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5 style="padding: 80px 0px 0px 0px; margin: -80px 0px 0px 0px;" #question-5>
                        How do I submit a design solution?
                  <div.card-inner>
                    <p>
                      When working on a design, in lower right corner you should
                      see a red "tools" button.
                      If you click on it, you should see a blue "submit" button.
                      Click on it, and you will see a submit dialog. Enter your
                      comments about the submission and press "submit" button.
                      You will be redirected to the main site page, and your design
                      will be saved.
                    <p>
                      You can come back and work on your design by using "menu ->
                      Work on a design" entry in qua-kit site.


            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5 style="padding: 80px 0px 0px 0px; margin: -80px 0px 0px 0px;" #question-6>
                        I still cannot submit my design!
                  <div.card-inner>
                    <p>
                      We kindly ask you to submit an issue, so we could resolve
                      it as soon as possible
                      (
                      <a href="@{FeedbackR}">@{FeedbackR}
                      ).
                    <p>
                      Could you please clarify following points? This is important
                      to find out exactly what your problem was.
                    <ul>
                      <li> Which link do you use to start the exercise? Note, at least
                        for a first time, you must use “Go!” button on the exercise 1 page.
                      <li> The browser you use (it will be very helpful if you can
                        find out the version too).
                      <li> The Operating System (Linux/Windows/Mac?)
                      <li> Are you able to move/rotate buildings?
                      <li> Do you have a blue submit button when you click on red
                        tools button?
                      <li> You do have a submit button, does the "submit design" dialog
                        window appear on click?
                      <li> What happens after you press "submit" in that window?

            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5 style="padding: 80px 0px 0px 0px; margin: -80px 0px 0px 0px;" #question-7>
                        I can only view a design, but not edit or submit it!
                  <div.card-inner>
                    <p>
                      This may happen if you use a wrong link to the editor
                        (or you are not logged in the system).
                    <p>
                      Make sure you use either of two ways:
                    <ul>
                      <li> Use a button "Go!" on the exercise page at edX
                        (it logs you in with edX credentials).
                      <li> If you are logged in currently (check it at upper left
                        corner of qua-kit site), you can use "menu" button
                        (upper left corner) -> "Work on a design".


            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5 style="padding: 80px 0px 0px 0px; margin: -80px 0px 0px 0px;" #question-8>
                        How do I find my submission after uploading it?
                  <div.card-inner>
                    <p>
                      All submissions are listed in the gallery
                      ("menu" -> "Explore submissions"),
                      though it might be difficult to find your own submission, as there
                      a lot of them.
                    <p>
                      On the other hand, you can always open your design in the
                      editing mode.

                      Just go to "menu" near top-left corner of any page at qua-kit
                      and then click on "work on a design".
                      Alternatively, you can use the same "Go!"
                      button as the first time. Both ways redirect you to your last submission.


            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5 style="padding: 80px 0px 0px 0px; margin: -80px 0px 0px 0px;" #question-9>
                        Some of my building blocks overlap. Is that ok?
                  <div.card-inner>
                    <p>
                      Yes. Although it is not an explicit feature, it is your decision
                      to shrink the total area of buildings by overlapping them.


            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5 style="padding: 80px 0px 0px 0px; margin: -80px 0px 0px 0px;" #question-10>
                        What do orange lines mean?
                  <div.card-inner>
                    <p>
                      Orange lines are meant as a guidance. They depict surrounding
                      roads, zones, and buildings.


            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5 style="padding: 80px 0px 0px 0px; margin: -80px 0px 0px 0px;" #question-11>
                        Which area should I use for placing building blocks?
                  <div.card-inner>
                    <p>
                      You should use an empty area in the center free of orange lines.


            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5 style="padding: 80px 0px 0px 0px; margin: -80px 0px 0px 0px;" #question-12>
                        How can I see rating of my proposal
                  <div.card-inner>
                    <p>
                      You can look it up in the gallery ("menu" -> "Explore submissions").
                    <p>
                      Icons and numbers show per-criterion rating on 0-100% scale
                      (it is not a number of votes).


            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5 style="padding: 80px 0px 0px 0px; margin: -80px 0px 0px 0px;" #question-13>
                        Can I comment others?
                  <div.card-inner>
                    <p>
                      Yes. You can click "view" on a selected submission, then write
                      a comment in the viewer windows.
                    <p>
                      You will also have to put up- or down-vote on a single criterion
                      for this design.


            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5 style="padding: 80px 0px 0px 0px; margin: -80px 0px 0px 0px;" #question-14>
                        How can I see if anybody left a comment on my design?
                  <div.card-inner>
                    <p>
                      Enter an editor mode ("menu" -> "Work on a design"), then
                      open menu -> control panel (cogwheel icon).
                    <p>
                      You can only respond in a form of changing the description
                      of your submission (when submitting a design update).


            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5 style="padding: 80px 0px 0px 0px; margin: -80px 0px 0px 0px;" #question-15>
                        Do I need to use all blocks provided at qua-kit design
                         template?
                  <div.card-inner>
                    <p>
                      Yes, you do. There is no way to remove or add a block from
                      the design.


            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5 style="padding: 80px 0px 0px 0px; margin: -80px 0px 0px 0px;" #question-16>
                        Can I update my design after submitting it?
                  <div.card-inner>
                    <p>
                      Yes, you can.
                    <p>
                      Just go into an editor mode ("menu" -> "Work on a design")
                      at qua-kit site and continue your work.
                    <p>
                      You can change it as many times as you want until the end
                      of the course.


            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5 style="padding: 80px 0px 0px 0px; margin: -80px 0px 0px 0px;" #question-17>
                        How can I indicate that I have completed the task or I
                        am still in progress?
                  <div.card-inner>
                    <p>
                      You cannot do so. All submissions are visible to everybody
                      right after you save them.
                      Therefore we strongly recommend you to finish the exercise
                      before the voting task starts (week 6).
                      At that time, you will vote for submissions of others.
                    <p>
                      Nevertheless, you still can update your design until the very
                      end of the course.
                      That is, you can respond to comments of other students by
                      addressing any issues they could have noticed.


            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5 style="padding: 80px 0px 0px 0px; margin: -80px 0px 0px 0px;" #question-18>
                        How do I get the grade for the first (design) exercise?
                  <div.card-inner>
                    <p>
                      Right after you submit your design you get 60% of your grade
                      automatically.
                    <p>
                      Then, your design is open for commenting and voting.
                      In the second exercise other students vote for your designs
                      and this make a peer-reviewed rating of all designs.
                      Based on this rating, you get additional 0-40% (worst to best
                      designs) of your grade.
                    <p>
                      Please note, voting is a long process so your grade will not
                      change immediately.
                      Instead, it will be updated once a day until the course ends
                      (to take into account even latest user votes).


            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5 style="padding: 80px 0px 0px 0px; margin: -80px 0px 0px 0px;" #question-19>
                        How do I get the grade for the second (compare-vote) exercise?
                  <div.card-inner>
                    <p>
                      Right after you finish minimum number of comparisons, you
                      get 60% of your grade automatically.
                    <p>
                      Remaning 0-40% you get while others vote: if your comparisons
                      votes is similar to majority of other votes, your grade gets higher;
                      if your votes contradict the majority, your grade gets lower.
                      This is the essence of croud-sourcing approach and citizen
                      science: majority decides!
                    <p>
                      Note, your grade is updated once a day as long as we receive
                      other votes (to take into account even latest user votes).


            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5 style="padding: 80px 0px 0px 0px; margin: -80px 0px 0px 0px;" #question-20>
                        I have not finished my design, but get (not good) grades!
                  <div.card-inner>
                    <p>
                      This is a common situation. Look which criteria should you
                      address more, and submit a new, revised version.
                    <p>
                      Your old votes get giscounted as new versions of your design
                      appear.
                      That is, if you submit a new version and get good responses,
                      they count more than previous towards your final rating.


            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5 style="padding: 80px 0px 0px 0px; margin: -80px 0px 0px 0px;" #question-21>
                        Can I have multiple submissions?
                  <div.card-inner>
                    <p>
                      No, only the last version of your design appears in qua-kit
                      gallery and is available for voting/grading.


            <div class="col-lg-12 col-md-12 col-sm-12 col-xs-12">
              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner>
                      <h5.h5 style="padding: 80px 0px 0px 0px; margin: -80px 0px 0px 0px;" #question-22>
                        What happens if I change my nickname at qua-kit?
                  <div.card-inner>
                    <p>
                      Nothing bad. Your design will still be connected to your account.
                    <p>
                      Your nickname appears in the submission gallery ("menu" ->
                      "Explore submissions").
                      Hence, if you change your nickname to match the one at edX,
                      it is easier to find and comment your design submission for others.
                      So, do it! :)

            <script>
              var newLine, el, ToC = "";
              \$("div h5").each(function() {
                el = $(this);
                ToC +=
                  "<li style='color: #ff6f00'>" +
                    "<a href='#" +  el.attr("id") + "'>" +  el.text() +
                    "</a>" +
                  "</li>";
              });
              \$("#outlineList").html(ToC);
        |]
