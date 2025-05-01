def replication:  String := "copy"
def manifestation: String := "output"
def meme: String := "medium-is-the-message"
def llm: String := "llm"

def artificial_intelligence: String := "artificial intelligence"
def artificial_life: String := "artificial life"
inductive Story: Type
| Epic : Story

def life: Story := Story.Epic
def prolog: Story := Story.Epic
def lisp: Story := Story.Epic
def intelligence: Story := Story.Epic
def nextStep (actor: Actor) (story: Story): Story := story
def epic
(story: Story)
(actor : Actor)
(al :Actor)
(ai :Actor)


:= story

def artificial (story: Story) (actor : Actor):= story
def contains (story: Story) (actor : Actor):= story
def story: Story := Story.Epic
--String := "epic"


def foo := do
  let task_list := [
      (epic story)
      ((artificial intelligence)
      (contains
       [(artificial life),
         (prolog),
        (lisp)
       ]))
      (meme)
      (medium-is-the-message)
      (llm)
      (manifestation),-- output
      (replication),  -- copy
]
