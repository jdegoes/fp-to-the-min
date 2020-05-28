## Introduction

This is the repository for the closing keynote of [Scala in the City Conference](https://www.eventbrite.co.uk/e/virtual-scala-in-the-city-conference-tickets-104448622642), held online May 28, 2020, and organized by [Signify Technology](http://signifytechnology.com/).

## FP to the Min

In 2018, John A. De Goes did a live coding session for the Functional Programming Group entitled, [FP to the Max](https://www.youtube.com/watch?v=sxudIMiOo68). Watched by tens of thousands of Scala developers, the video helped many understand the benefits of the total, deterministic functions. Yet, due to its use of type classes, higher-kinded types, and implicits, many developers were wondering: is this style of programming practical?

In this highly anticipated sequel to _FP to the Max_, John shows the sweet spot for functional programming in Scala: a magical place that provides most of the benefits of _FP to the Max_, but in a tiny fraction of the time and effort, with no jargon, no boilerplate, and no ceremony. This is the place that gets colleagues excited about using Scala and helps companies realize the full potential of the powerful programming language. Come discover how functional programming can help you be more productive and write better code that follows best practices, in a fraction of the time required by other approaches!

### Agenda 

 - Introduction
   - FP to the Max
     - Intro to FP
     - Benefits in the Large
     - Overkill?
   - FP to the Min
     - Skip the Intro
     - Deeper Benefits in the Large
     - Underkill?
 - FP to the Min
   - Domain Introduction
   - Challenge Review
     - Bug Reports
     - Feature Requests
   - Incremental Refactoring
     - Functional Effects
     - Areas
       - Error Management
       - Concurrency & Parallelism
       - Resource Management
       - Dependency Management
       - Testing
   - Review

## The Source Code

The example code used during the presentation can be found in the `fpmin` subdirectory.

As a bonus, the original code from _FP to the Max_ can be found inside the package `fpmin.fpmax`!

## Challenges

The application is "working" but has a number of known bugs and pending feature requests.

Your mission, if you choose to accept it, is to fix the bugs and implement the feature requests!

Can you use the power of functional programming to complete this task, without:

 - Higher-kinded types
 - Functional type classes
 - Implicits
 - Scary jargon

Good luck!

### Changes

1. The application leaks file descriptors and eventually stops working.
2. If some day's worth of data is missing, no statistics will ever be collected.
3. If the application is sensitive to transient problems in the Github API.
4. The application should download and aggregate data files in parallel.
5. The application should support a timeout on file downloads so it doesn't hang forever.
6. If the application is still downloading the files from a previous run, it should not start a new run.



