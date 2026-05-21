---
layout: post
title: "What an AI Engineer Actually Does"
tags: [ai, ai-engineering, llm, product, mlops]
---

<style>
  .ai-visual {
    margin: 2rem 0;
    padding: 1.5rem;
    border: 1px solid #e6e8ef;
    border-radius: 18px;
    background: linear-gradient(135deg, #fbfcff 0%, #f4f7fb 100%);
    box-shadow: 0 10px 30px rgba(31, 45, 61, 0.08);
  }

  .ai-visual h4 {
    margin-top: 0;
    margin-bottom: 1rem;
    color: #24324b;
  }

  .ai-scope-grid,
  .ai-responsibility-grid,
  .ai-week-grid,
  .ai-skills-grid {
    display: grid;
    gap: 1rem;
  }

  .ai-scope-grid {
    grid-template-columns: repeat(auto-fit, minmax(170px, 1fr));
  }

  .ai-scope-card,
  .ai-responsibility-card,
  .ai-week-card,
  .ai-skill-card {
    padding: 1rem;
    border-radius: 14px;
    background: #fff;
    border: 1px solid #e8ebf2;
  }

  .ai-scope-card strong,
  .ai-responsibility-card strong,
  .ai-week-card strong,
  .ai-skill-card strong {
    display: block;
    margin-bottom: 0.35rem;
    color: #22304a;
  }

  .ai-center-node {
    margin: 1rem auto;
    width: min(100%, 420px);
    padding: 1rem;
    border-radius: 999px;
    text-align: center;
    color: #fff;
    font-weight: 700;
    background: linear-gradient(135deg, #3156d4, #7c3aed);
  }

  .ai-responsibility-grid {
    grid-template-columns: repeat(auto-fit, minmax(210px, 1fr));
  }

  .ai-responsibility-card span {
    display: inline-block;
    margin-bottom: 0.5rem;
    padding: 0.2rem 0.55rem;
    border-radius: 999px;
    background: #eef2ff;
    color: #3156d4;
    font-size: 0.85rem;
    font-weight: 700;
  }

  .ai-week-grid {
    grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
  }

  .ai-week-card {
    border-top: 5px solid #3156d4;
  }

  .ai-role-table {
    width: 100%;
    border-collapse: collapse;
    background: #fff;
    border-radius: 14px;
    overflow: hidden;
  }

  .ai-role-table th,
  .ai-role-table td {
    padding: 0.9rem;
    border-bottom: 1px solid #e8ebf2;
    vertical-align: top;
  }

  .ai-role-table th {
    background: #24324b;
    color: #fff;
  }

  .ai-skills-grid {
    grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
  }

  .ai-demo-vs-prod {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
    gap: 1rem;
  }

  .ai-demo-vs-prod ul {
    margin-bottom: 0;
  }
</style>

And what the week can look like when the job is less “chatbot magic” and more “making the thing actually work.”

There’s a funny thing happening right now.

Everyone wants AI.

Executives want AI roadmaps. Product teams want AI features. Data scientists want LLMs in their workflows. Engineers want agents that do useful things instead of expensive demos. Vendors want to sell you “autonomous intelligence” wrapped in a glossy UI.

And somewhere in the middle of all of that is the AI Engineer.

The person who is supposed to take a fuzzy sentence like:

> “Can we use AI to automate this workflow?”

And turn it into something that runs, scales, gets evaluated, doesn’t leak data, doesn’t hallucinate its way into production, and is actually useful to the person on the other side of the screen.

That is the job.

Not prompt wizard. Not traditional data scientist. Not pure backend engineer. Not MLOps engineer, although there is overlap. Not product manager, although sometimes it feels like it.

An AI Engineer sits in the messy middle between models, systems, users, workflows, and business value.

This post is my attempt to explain what that role actually does.

### 1) Why this role feels confusing

Before AI Engineering became a title, a lot of us were already doing pieces of it.

Maybe you were a data scientist building models. Maybe you were an analytics engineer wiring up trusted data products. Maybe you were an MLOps engineer deploying pipelines. Maybe you were a backend engineer integrating APIs. Maybe you were the person who kept getting asked:

> “Can we just use ChatGPT for this?”

And then somehow you became the person responsible for figuring out what “this” actually meant.

That is why AI Engineering is hard to define.

The role didn’t emerge cleanly from a textbook. It emerged from the collision of several things:

- LLMs became useful enough to put into products.
- APIs made model access easy.
- Enterprises realized the model is only 10% of the problem.
- Workflows became the real battleground.
- Everyone discovered that demos are cheap, but production is expensive.

A prototype can be built in an afternoon.

A production AI system needs data access, retrieval, evaluation, permissioning, observability, latency controls, cost controls, user feedback loops, security review, deployment patterns, fallback behavior, human-in-the-loop workflows, and some honest thinking about whether AI should be involved at all.

That gap between “cool demo” and “trusted product” is where the AI Engineer lives.

The article this is loosely inspired by frames MLOps Engineers as builders and maintainers of tooling and infrastructure that support data science development and deployment. I think AI Engineering has a similar “bridge” quality, but the center of gravity has shifted from model productionization to AI productization.

### 2) So what is an AI Engineer?

Here is my working definition:

> An AI Engineer designs, builds, evaluates, and productionizes AI-powered systems that help humans complete real workflows better, faster, or with more leverage.

The important word is systems.

An AI Engineer is rarely just calling a model. They are building the scaffolding around the model so that the system can do something useful.

That might mean:

- turning unstructured user intent into structured actions
- connecting LLMs to tools, APIs, databases, or documents
- building retrieval pipelines
- designing agent workflows
- creating evals to measure output quality
- building human-in-the-loop review systems
- adding guardrails and fallback logic
- deploying services with real authentication and monitoring
- making sure the AI output maps to a business process

The model is part of the system.

But the system is the product.

<div class="ai-visual">
  <h4>Scope of AI Engineering</h4>
  <div class="ai-center-node">AI Engineer: the bridge from capability to workflow</div>
  <div class="ai-scope-grid">
    <div class="ai-scope-card"><strong>Models</strong>LLMs, embeddings, classifiers, tool use, structured outputs.</div>
    <div class="ai-scope-card"><strong>Context</strong>Documents, metadata, SQL, APIs, permissions, freshness.</div>
    <div class="ai-scope-card"><strong>Systems</strong>Services, queues, data stores, orchestration, deployment.</div>
    <div class="ai-scope-card"><strong>Evaluation</strong>Golden sets, human labels, acceptance, cost, latency, quality.</div>
    <div class="ai-scope-card"><strong>Workflow</strong>Users, review loops, approvals, overrides, downstream actions.</div>
    <div class="ai-scope-card"><strong>Trust</strong>Security, auditability, fallbacks, observability, governance.</div>
  </div>
</div>

### 3) What does an AI Engineer own?

I’d group the work into five buckets.

<div class="ai-visual">
  <h4>Breakdown of responsibilities</h4>
  <div class="ai-responsibility-grid">
    <div class="ai-responsibility-card"><span>01</span><strong>Product translation</strong>Turn fuzzy requests into workflow-shaped problems with clear users, decisions, constraints, and success measures.</div>
    <div class="ai-responsibility-card"><span>02</span><strong>Data and context engineering</strong>Design the information environment the model needs: retrieval, APIs, metadata, governance, and freshness.</div>
    <div class="ai-responsibility-card"><span>03</span><strong>Application and workflow engineering</strong>Wire models into services, internal tools, review queues, pipelines, and deployed product experiences.</div>
    <div class="ai-responsibility-card"><span>04</span><strong>Evaluation</strong>Measure whether the system is correct, useful, trusted, fast enough, cheap enough, and improving over time.</div>
    <div class="ai-responsibility-card"><span>05</span><strong>Operations, governance, and trust</strong>Make the system observable, permission-aware, auditable, reliable, reversible, and safe enough to use.</div>
  </div>
</div>

#### Product translation

This is where the AI Engineer asks:

What is the actual job-to-be-done?

Not “we need an AI agent.”

More like:

- Who is the user?
- What decision are they trying to make?
- What information do they need?
- What do they currently do manually?
- What part of this workflow is repetitive, ambiguous, or judgment-heavy?
- What would make the output trustworthy?
- Where does a human still need final say?

This is the part people underestimate.

A lot of bad AI products start because someone skips straight to the model.

The better move is usually slower: first understand the workflow. Then decide where intelligence belongs.

Sometimes AI should generate. Sometimes it should classify. Sometimes it should retrieve. Sometimes it should summarize. Sometimes it should recommend. Sometimes it should simply make a tedious UI better.

And sometimes the answer is: do not use AI here.

That is also AI Engineering.

Knowing when not to use the thing.

#### Data and context engineering

LLMs are powerful, but they are not mind readers.

Most enterprise AI systems fail because the model does not have the right context, not because the model is “bad.”

So a huge part of AI Engineering is context design.

That means figuring out:

- What data does the model need?
- Where does that data live?
- Is it structured or unstructured?
- Is it fresh?
- Is it governed?
- Who is allowed to see it?
- Should we use RAG, fine-tuning, rules, SQL, APIs, or some hybrid?
- How do we turn messy business data into something the model can reliably use?

In traditional data science, feature engineering was often the craft.

In AI Engineering, context engineering is becoming the craft.

The work is not just “write a better prompt.”

It is: build the right information environment so the model has a fighting chance.

That might mean creating a clean product metadata layer. Or building a document retrieval system. Or writing a tool that lets an agent query BigQuery safely. Or creating a structured taxonomy so AI outputs map to real business categories.

The model is only as useful as the context you give it.

Garbage in, hallucination out.

#### Application and workflow engineering

This is where the AI Engineer becomes a builder.

You are not just exploring notebooks anymore.

You are wiring things into an actual product or process.

That could include FastAPI services, Cloud Run apps, background jobs, vector databases, Postgres tables, queues, event triggers, React or internal UIs, auth layers, tool-calling APIs, data pipelines, Slack bots, review dashboards, and deployment workflows.

This is the less glamorous part of AI Engineering.

It is also where most of the value is created.

Because a great model trapped in a notebook is a research artifact.

A decent model embedded into the right workflow can change how a team operates.

This is why AI Engineers need enough software engineering taste to ask:

- How does this get deployed?
- How does this fail?
- How does this recover?
- How does a user correct it?
- How do we log what happened?
- How do we know whether it worked?
- How do we keep cost from going sideways?
- How do we make it boring enough to trust?

The goal is not to make AI feel magical.

The goal is to make it useful enough that people stop thinking about the AI and just use the workflow.

#### Evaluation

This might be the most important bucket.

AI systems are strange because they can look amazing in demos and quietly degrade in production.

They can be correct in tone and wrong in substance.

They can pass five examples and fail the sixth in a way that makes everyone uncomfortable.

So the AI Engineer has to build evaluation into the system early.

Not as an afterthought.

Evals can include:

- golden datasets
- human review labels
- precision and recall
- hallucination checks
- rubric-based grading
- latency tracking
- cost per task
- user acceptance rates
- override rates
- fallback rates
- win/loss analysis against the old process

For AI products, “accuracy” is often not enough.

You need to know:

- Did the user trust it?
- Did they edit the output?
- Did it save time?
- Did it make the decision better?
- Did it create new risk?
- Did it fail gracefully?
- Did it improve with feedback?

This is where my data science brain still feels useful.

The AI Engineer should not just ship the feature.

They should design the learning loop.

A good AI system gets better because the system captures feedback in a way that can be used.

Bad AI systems just collect vibes.

#### Operations, governance, and trust

Every AI system eventually runs into the adult problems.

Security. Permissions. PII. Cost. Reliability. Versioning. Monitoring. Auditability. Stakeholder trust.

This is where the job starts to overlap with MLOps, platform engineering, and enterprise architecture.

The AI Engineer needs to care about:

- which model version produced an answer
- which prompt template was used
- what documents were retrieved
- what tool calls were made
- whether the user had permission to access that data
- whether the answer can be reproduced
- whether feedback can be traced
- whether the system can be rolled back
- whether failures are visible

This is also where the hype gets humbled.

Because a user does not care that your agent architecture is elegant.

They care that it worked when they needed it.

They care that it did not expose something it should not.

They care that it did not confidently recommend nonsense.

Trust is not a model setting.

Trust is an engineering outcome.

### 4) What does the week actually look like?

The week depends heavily on the company.

At a startup, an AI Engineer might be building the whole thing end to end:

- Monday: talk to users
- Tuesday: build the prototype
- Wednesday: wire up the API
- Thursday: deploy it
- Friday: realize the evals are bad and rebuild half of it

At a larger company, the week might be more distributed: stakeholder meetings, architecture reviews, data access conversations, prototype development, prompt and retrieval experiments, eval design, security review, deployment work, documentation, bug fixing, onboarding users, and explaining why the demo is not production-ready yet.

A realistic AI Engineering week might look something like this:

<div class="ai-visual">
  <h4>A realistic AI Engineering week</h4>
  <div class="ai-week-grid">
    <div class="ai-week-card"><strong>Monday</strong>Clarify the workflow and separate “AI assistant” from the real operational problem.</div>
    <div class="ai-week-card"><strong>Tuesday</strong>Design the system: deterministic rules, probabilistic steps, human review, logging, and writeback.</div>
    <div class="ai-week-card"><strong>Wednesday</strong>Build the ugly first version that proves the workflow can move end to end.</div>
    <div class="ai-week-card"><strong>Thursday</strong>Test against messy real data, edge cases, security constraints, latency, and cost.</div>
    <div class="ai-week-card"><strong>Friday</strong>Close the loop by measuring whether the workflow improved and what feedback was captured.</div>
  </div>
</div>

#### Monday: clarify the workflow

You meet with the business team.

They say:

> “We want an AI assistant.”

You ask:

> “Assistant for what?”

Eventually, after peeling the onion, the real problem appears:

They have 5,000 products that need review. The taxonomy is inconsistent. The nutrition data is incomplete. The subject matter experts are overwhelmed. The business wants a faster way to classify, explain, and approve decisions.

Now you have something real.

Not an AI assistant.

A workflow.

#### Tuesday: design the system

You sketch the architecture.

Maybe it looks like:

- source data table
- preprocessing pipeline
- LLM classification step
- rules engine
- explanation layer
- human review UI
- feedback capture
- writeback table
- downstream publishing process

This is where AI Engineering becomes systems thinking.

You are asking:

- What should be deterministic?
- What should be probabilistic?
- Where do rules beat models?
- Where does the model help?
- Where does the human need final say?
- What gets logged?
- What becomes training or evaluation data later?

The best AI systems are usually not pure AI.

They are hybrids.

Rules where you need precision. Models where you need judgment. Humans where you need accountability. Pipelines where you need repeatability.

#### Wednesday: build the ugly first version

This is the part I enjoy.

Get something working.

Not perfect. Not polished. Working.

Maybe it is a FastAPI endpoint. Maybe it is a notebook. Maybe it is a Streamlit app. Maybe it is a Cloud Run service. Maybe it is an internal review table. Maybe it is a script that connects a model to a workflow for the first time.

The trick is to move fast without lying to yourself.

There is a difference between a prototype and a foundation.

A prototype answers:

> Is this possible?

A foundation answers:

> Can we build on this without hating ourselves later?

AI Engineers live in that tension.

Move fast enough to learn. Build clean enough to not create a swamp.

#### Thursday: test against reality

This is where the AI demo usually gets humbled.

The happy path worked.

But then real data shows up.

The product name is weird. The document is badly formatted. The category is ambiguous. The model gives an explanation that sounds good but is wrong. The API times out. The stakeholder asks for a batch export. The reviewer wants a reason code. The security team asks where the data is going. The cost estimate looks spicy.

This is not failure.

This is the job.

AI Engineering is largely the art of dragging AI out of the demo garden and into the weather.

#### Friday: close the loop

By the end of the week, the question is not:

> “Did we use AI?”

The question is:

> “Did we make the workflow better?”

That might mean:

- reviewers processed more items
- decisions were more consistent
- explanations were clearer
- edge cases were easier to find
- humans spent less time on obvious cases
- the system captured better feedback
- the next model version has better evaluation data

This is where the role becomes satisfying.

Not because the AI was impressive.

Because the system created leverage.

### 5) AI Engineer vs Data Scientist vs MLOps Engineer

The boundaries are blurry, but here is how I think about it.

<div class="ai-visual">
  <h4>Role comparison</h4>
  <table class="ai-role-table">
    <thead>
      <tr>
        <th>Role</th>
        <th>Core question</th>
        <th>Center of gravity</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td><strong>Data Scientist</strong></td>
        <td>What can we learn from the data, and can we model or measure this decision?</td>
        <td>Analysis, modeling, experimentation, measurement.</td>
      </tr>
      <tr>
        <td><strong>MLOps Engineer</strong></td>
        <td>How do we reliably develop, deploy, monitor, and maintain ML systems?</td>
        <td>Pipelines, infrastructure, observability, lifecycle management.</td>
      </tr>
      <tr>
        <td><strong>AI Engineer</strong></td>
        <td>How do we turn AI capabilities into useful, trustworthy products and workflows?</td>
        <td>Productized AI systems, context, evals, workflow integration, trust.</td>
      </tr>
    </tbody>
  </table>
</div>

A Data Scientist asks:

> What can we learn from the data, and can we model or measure this decision?

An MLOps Engineer asks:

> How do we reliably develop, deploy, monitor, and maintain ML systems?

An AI Engineer asks:

> How do we turn AI capabilities into useful, trustworthy products and workflows?

There is overlap everywhere.

A good AI Engineer borrows from all three worlds:

- From data science: experimentation, measurement, evaluation, statistical humility.
- From software engineering: APIs, services, testing, deployment, reliability.
- From MLOps: pipelines, observability, reproducibility, model lifecycle management.
- From product: user empathy, workflow design, prioritization, adoption.
- From design: interaction patterns, feedback loops, trust-building.

That is what makes the role exciting.

And also why it can feel chaotic.

You are rarely just doing one job.

You are translating across several.

### 6) The skills that matter most

The obvious skills are technical: Python, APIs, SQL, cloud infrastructure, LLM APIs, RAG, vector search, prompt design, orchestration, eval frameworks, basic frontend or internal tools, CI/CD, and observability.

But the less obvious skills might matter more.

<div class="ai-visual">
  <h4>The underrated skill stack</h4>
  <div class="ai-skills-grid">
    <div class="ai-skill-card"><strong>Taste</strong>Knowing when the model is being used as a hammer and when a simple rule is better.</div>
    <div class="ai-skill-card"><strong>Judgment</strong>Making decisions under uncertainty about quality, risk, and human review.</div>
    <div class="ai-skill-card"><strong>Translation</strong>Speaking business, data, engineering, model behavior, and user workflow.</div>
    <div class="ai-skill-card"><strong>Debugging</strong>Finding whether the problem is code, data, retrieval, prompt, process, or expectation.</div>
    <div class="ai-skill-card"><strong>Restraint</strong>Putting intelligence where it compounds instead of putting agents everywhere.</div>
  </div>
</div>

#### Taste

Knowing when something is over-engineered.

Knowing when the model is being used as a hammer.

Knowing when a simple rule is better.

Knowing when the demo is lying.

Knowing when the user experience is too magical and not transparent enough.

#### Judgment

AI systems are probabilistic.

That means someone has to make decisions under uncertainty.

How good is good enough? Where do we need human review? What failure modes are acceptable? What risks are not worth taking?

#### Translation

You have to speak business, data, engineering, and model behavior.

One minute you are discussing JSON schemas.

The next minute you are explaining to a stakeholder why “accuracy” does not mean what they think it means.

The next minute you are helping an engineer understand why the user needs an override reason.

#### Debugging

AI debugging is weird.

Sometimes the bug is code. Sometimes it is data. Sometimes it is retrieval. Sometimes it is the prompt. Sometimes it is the user’s actual process being inconsistent. Sometimes it is the model doing exactly what you asked but not what you meant.

#### Restraint

The best AI Engineers are not trying to put agents everywhere.

They are trying to put intelligence where it compounds.

### 7) The trap of the “AI demo”

The easiest thing in AI is a demo.

The hardest thing is a dependable workflow.

<div class="ai-visual">
  <h4>AI theater vs AI engineering</h4>
  <div class="ai-demo-vs-prod">
    <div class="ai-scope-card">
      <strong>AI theater optimizes for applause</strong>
      <ul>
        <li>Five clean examples</li>
        <li>No permissions</li>
        <li>Hidden latency</li>
        <li>No evaluation</li>
        <li>Edge cases ignored</li>
      </ul>
    </div>
    <div class="ai-scope-card">
      <strong>AI engineering optimizes for trust</strong>
      <ul>
        <li>Messy real inputs</li>
        <li>Permission-aware context</li>
        <li>Observable failures</li>
        <li>Measured quality</li>
        <li>Human feedback loops</li>
      </ul>
    </div>
  </div>
</div>

A demo can ignore permissions. A demo can use five clean examples. A demo can hide latency. A demo can skip evaluation. A demo can pretend edge cases do not exist. A demo can make every output look useful because no one is forced to use it.

Production is different.

Production asks:

- What happens when the input is bad?
- What happens when the model is wrong?
- What happens when the user disagrees?
- What happens when the API is down?
- What happens when the answer affects a real business decision?
- What happens when someone asks, “Why did it say that?”

That is the difference between AI theater and AI engineering.

AI theater optimizes for applause.

AI engineering optimizes for trust.

### 8) What makes this role fun

The fun part is that AI Engineering feels like building little machines for human leverage.

You are not just predicting churn. You are not just building dashboards. You are not just deploying models.

You are building systems that can read, reason, retrieve, classify, summarize, recommend, and act.

And when it works, it feels like you gave a team a new gear.

A reviewer can move faster. A product manager can explore more ideas. A data scientist can ship with less friction. An operator can stop copying and pasting between systems. A customer can get an answer without waiting on a human.

That is the magic.

Not the model.

The leverage.

### 9) Closing thought

AI Engineering is not about being the person who knows the newest model name.

That changes every few months.

It is about being the person who can take a new capability and turn it into a working system.

A system that respects the user. A system that fits the workflow. A system that can be measured. A system that can fail safely. A system that earns trust over time.

The title will probably keep changing.

AI Engineer. Applied AI Engineer. LLM Engineer. AI Product Engineer. Agent Engineer. Full-stack AI Builder.

Whatever.

Titles are useful, but imperfect. The real question is simpler:

> Can you build the bridge between what AI can do and what people actually need?

That is the job.

And honestly, it is a pretty fun one.