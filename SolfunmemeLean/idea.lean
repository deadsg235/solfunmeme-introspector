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
def epic (story: Story) := story

def artificial (story: Story)  : Story:= story
def contains (stories: List  Story) : Story:= Story.Epic
def story: Story := Story.Epic
--String := "epic"


def foo2 : Story:=
  let al : Story := (artificial life)
  let l1 : List Story:= [
          prolog,
          al,
          lisp
        ]
  let co := contains l1
  co


def foo := do

  let task_list : List Story := [
      (epic story),
      (artificial intelligence),
      (meme),
      (medium_is_the_message),
      (llm),
      (manifestation),-- output
      (replication),  -- copy
]
