inductive Story: Type
| Epic : Story

def replication:  Story := Story.Epic
def manifestation: Story := Story.Epic
def meme : Story := Story.Epic
def llm: Story := Story.Epic

def artificial_intelligence: Story := Story.Epic
def artificial_life: Story := Story.Epic
def medium_is_the_message: Story := Story.Epic

def life: Story := Story.Epic
def prolog: Story := Story.Epic
def lisp: Story := Story.Epic
def intelligence: Story := Story.Epic
--def nextStep (actor: Actor) (story: Story): Story := story
def epic (story: Story) := story

def artificial (story: Story)  : Story:= story
def contains (_stories: List  Story) : Story:= Story.Epic
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


def foo : List Story :=

  let task_list : List Story := [
      --(epic story),
      --(artificial intelligence),
      --(meme),
      --(medium_is_the_message),
      --(llm),
      --(manifestation),-- output
 --     (replication),  -- copy
 ]
task_list
