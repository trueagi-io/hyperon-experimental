from hyperon import *
import numpy as np
import pandas as pd
from torchvision import datasets
from torchvision.transforms import ToTensor
from enum import Enum
from sklearn.metrics.pairwise import cosine_similarity

SEGMENT = 10
MATCH_RATIO = 0.95
NODE_ID = 0
LINK_ID = 0

# Initial data preparation
data_motivation = {
    'Time': [1], 
	'Motivation': ['Reflex'],
	'Need': ['Pay Attention on Unusual Event'],
	'Action': ['Research Unusual Event'],
    'Result': ['Understand Unusual Event'], 
    'Status': [[['Visual', 'Other'],['Hearing', 'Other'],['Touch', 'Other']]]
    }
Motivation_Global = pd.DataFrame(data_motivation)

data_action = {
    'Name': ['Research Unusual Event', 'Thinking'],
	'Action_Type': [['Eye', 'Ear','Hand'],['Brain']], #ActionType
	'Action': [[['Look Forward','Look Left','Look Right'], ['Hearing Forward'], ['Hand Forward','Palm Open','Palm Close']],['Imagine']],
    'Digital_Twin_Index': [[[1,2,3], [4], [5,6,7]], [101]] }
Action_Global = pd.DataFrame(data_action)

# for Visual it's possible to translate Forward reaction to Left and Right using an algorith and to not save all variants
data_environment = {
    'Time': [1, 1, 1], 
	'Action': ['Look Forward', 'Hearing Forward', 'Hand Forward'],
    'Reaction_Type': ['Visual', 'Hearing', 'Touch'], #ReactionType
    'Reaction': ['Toy Forward', 'Look at Toy', 'Hand Straighten to Toy'] }
Environment_Global = pd.DataFrame(data_environment)

data_environment_reaction = {
	'Reaction': ['Toy Forward','Look at Toy','Hand Straighten to Toy'], 
    'Reaction_Type': ['Visual', 'Hearing', 'Touch'], #ReactionType
    'Digital_Twin_Index': [1, 101, 201] } # Digital Twin Index - index to real training data 
Environment_Global_reaction = pd.DataFrame(data_environment_reaction)

data_result = {
    'Name': ['Understand Unusual Event'],
    'Result_Type': [['Visual', 'Hearing', 'Touch']], #ResultType
	'Result': [[['Object is identified'],['Object is identified'],['Object in palm']]] }
Result_Global = pd.DataFrame(data_result)

# Example of using training data that can be prepared for concrete tasks
training_data = datasets.FashionMNIST(
    root="data",
    train=True,
    download=True,
    transform=ToTensor()
)

# Classes  
class ReactionType(Enum):
    Visual = 'Visual'
    Hearing = 'Hearing'
    Touch = 'Touch'
    Smell = 'Smell'
    Taste = 'Taste'
    Action = 'Action' # kinesthetic

class ResultType(Enum):
    Visual = 'Visual'
    Hearing = 'Hearing'
    Touch = 'Touch'
    Smell = 'Smell'
    Taste = 'Taste'
    Action = 'Action' # kinesthetic

class ActionType(Enum):
    Eye = 'Eye'
    Ear = 'Ear'
    Hand = 'Hand'
    Brain = 'Brain'
    Default = 'Default'

class Color(Enum):
    Other = 'Other'
    Red = 'Red'
    Yellow = 'Yellow'
    Green = 'Green'
    Blue = 'Blue'

class Size(Enum):
    Other = 'Other'
    Large = 'Large' 
    Medium = 'Medium' 
    Small = 'Small'

class Shape(Enum):
    Other = 'Other'
    Circle = 'Circle'
    Square = 'Square'
    Triangle = 'Triangle'
    Rectangle = 'Rectangle'

class Space(Enum):
    Other = 'Other'
    Length = 'Length'
    Height = 'Height'
    Width = 'Width'
    Up = 'Up'
    Down = 'Down'
    Right = 'Right'
    Left = 'Left'

class NodeType(Enum):
    Other = 'Other'
    Need = 'Need' # need or goal triggers action to achieve result 
    Action = ActionType
    Reaction = ReactionType
    Object = 'Object' # consists of different types of sensories
    Skill = 'Skill' # contains sequence of actions to achieve result
    Symbol = 'Symbol' # defines a name as something inherent in the Object being called
    Class = 'Class' # grouping of similar objects
    Operation = 'Operation' # awareness of skills
    Statement = 'Statement' # thinking about operations. As a result, a description of the solution algorithm in words (verbal hypotheses).


class LinkType(Enum):
    Other = 'Other'
    Previous = 'Previous'
    Next = 'Next'
    Together = 'Together'
    PartOf = 'PartOf'
    WholeFor = 'WholeFor'
    ObjectFor = 'ObjectFor'
    Above = 'Above'
    Below = 'Below'
    Larger = 'Larger'
    Smaller = 'Smaller'
    Far = 'Far'
    Closer = 'Closer'

class StateType(Enum):
    Other = 'Other'
    Predicted = 'Predicted'
    Active = 'Active'
    NonActive = 'NonActive'

class MemoryNode:
    def __init__(self):
        self.Node_ID = 0
        self.Key_Code = np.zeros(SEGMENT, dtype=float)
        self.Link_IDs = []
        self.Name = ""
        self.Type = NodeType.Other
        self.Match_Ratio = 0
        self.State = StateType.Other

class LinksEdge:
    def __init__(self):
        self.Edge_ID = 0
        self.Source_Node_ID = 0
        self.Dist_Node_ID = 0
        self.Weights = np.zeros(SEGMENT, dtype=float)
        self.Name = ""
        self.Type = LinkType.Other


# Global dict to save graph in Pyhton format, next step is to save in Rust as Grounded Atoms
Memory_Node_temp = MemoryNode()
Links_Edge_temp = LinksEdge()
Memory_Node_Global = {0: Memory_Node_temp}
Memory_Link_Global = {0: Links_Edge_temp}
# copy Atoms reference to Attention for fast search
Attention_Node_Global = {0: Memory_Node_temp}
Attention_Link_Global = {0: Links_Edge_temp}


# Functions for Rust integration with Space and Atoms. Rust functions are implemented but not incorporated in interface libraries.
# Memory_Space_Global_Rust = GroundingSpace() 

def identify_code_in_memory(Memory_code):
    # Rust - pub fn identify_code_in_memory(memory_space: & mut GroundingSpace, new_node_atom: &Atom) -> Atom {
    # ToDo - Try to find in Attention_Node_Global first, If no success then find in Memory_Node_Global
    global NODE_ID
    Is_Found = False
    for Node_ID, Memory_Node in Memory_Node_Global.items():
        Match_Ratio = match_ratio(Memory_Node, Memory_code)
        if (Match_Ratio >= MATCH_RATIO) & (Memory_code.Type==Memory_Node.Type):
            Is_Found = True
            Memory_code = Memory_Node
            break
    if not Is_Found:
        NODE_ID = NODE_ID + 1
        Memory_code.Node_ID = NODE_ID
        Memory_Node_Global[Memory_code.Node_ID] = Memory_code
        Memory_code = Memory_Node_Global[Memory_code.Node_ID]
    return Memory_code

def set_link_for_nodes(Prev_code, Next_code, Link_Type):
    # Rust - pub fn set_link_for_nodes(link_space: &mut GroundingSpace, prev: & Atom, next: & Atom) -> Vec<Atom> {
    global LINK_ID
    Is_Found = False
    Memory_arr = []
    Memory_Link_temp = LinksEdge()
    for Edge_ID, Memory_Link in Memory_Link_Global.items():
        if (Prev_code.Node_ID==Memory_Link.Source_Node_ID) & (Next_code.Node_ID==Memory_Link.Dist_Node_ID) & (Memory_Link.Type==Link_Type):
            Is_Found = True
            Memory_Link_temp = Memory_Link
            break
    if not Is_Found:
        LINK_ID = LINK_ID + 1
        Memory_Link_temp.Edge_ID = LINK_ID
        Memory_Link_temp.Source_Node_ID = Prev_code.Node_ID
        Memory_Link_temp.Dist_Node_ID = Next_code.Node_ID
        Memory_Link_temp.Type = Link_Type
        Memory_Link_temp.Weights = calc_wieght_increase(Memory_Link_temp.Weights)
        Memory_Link_Global[Memory_Link_temp.Edge_ID] = Memory_Link_temp
        Prev_code.Link_IDs.append(Memory_Link_temp.Edge_ID)
        Memory_Node_Global[Prev_code.Node_ID] = Prev_code
        Next_code.Link_IDs.append(Memory_Link_temp.Edge_ID)
        Memory_Node_Global[Next_code.Node_ID] = Next_code
    Memory_arr.append(Memory_Link_temp)
    Memory_arr.append(Prev_code)
    Memory_arr.append(Next_code)
    return Memory_arr

def find_linked_edges(Source_Node, Link_Type):
    # Rust - pub fn find_linked_edges(memory_space: & GroundingSpace, find_node_atom: &Atom) -> Vec<Atom> {
    Memory_arr = []
    for Node_Edge_ID in Source_Node.Link_IDs:
        for Edge_ID, Memory_Link in Memory_Link_Global.items():
            if (Edge_ID == Node_Edge_ID) & (Memory_Link.Type==Link_Type) :
                Memory_arr.append(Memory_Link)
    Memory_arr.sort(key = lambda x: x.Weights.sum())    
    return Memory_arr

def match_ratio(data, pattern):
    # Rust - pub fn match_ratio(data: &Atom, pattern: &Atom) -> f32 {
    # Match_Ratio = (cosine_similarity([data.Key_Code], [pattern.Key_Code])).item(0)
    Sum = data.Key_Code + pattern.Key_Code
    Diff = np.absolute(data.Key_Code - pattern.Key_Code)
    if Sum.sum() != 0:
        Match_Ratio = 1 - Diff.sum() / Sum.sum()
    else:
        Match_Ratio = 0
    return Match_Ratio

def calc_wieght_increase(Weights):
    # Rust - pub fn calc_wieght_increase(weights: & mut Vec<f32>, update_value: f32){
    if Weights.sum()==0:
        Temp = np.arange(SEGMENT, dtype=float) 
        Weights = np.full_like(Temp, 0.01)
    else:
        Weights = Weights * 0.01
    return Weights


# Business logic of curiosity behaviour of Child
def curiosity_behavior(Start, End):
    for t in range(Start, End+1):
        Motivation_item = Motivation_Global[Motivation_Global['Time']==t]
        if Motivation_item.size != 0:
            Is_Result_Achieved = PDCA_func(Motivation_item)
            if Is_Result_Achieved:
                print("Result is achieved! Time: ", t)
            t = t + 1
    print("curiosity_behavior ends.")

# Plan-Do-Check-Act approach to achieve result from goal
def PDCA_func(Motivation_item):
    Need_code_arr, Action_code_arr = Plan_Do_func(Motivation_item)
    Reaction_Result_arr = Do_Check_func(Action_code_arr, Motivation_item)
    Is_Result_Achieved_arr = Check_Act_func(Motivation_item, Reaction_Result_arr, Action_code_arr)
    for i in range(0, len(Is_Result_Achieved_arr)):
        if not Is_Result_Achieved_arr[i]:
            return False        
    return True 

def Plan_Do_func(Motivation_item):
    Need_code_arr = env_organs_to_memory_code(Motivation_item['Need'].iloc[0], NodeType.Need)
    for i in range(0, len(Need_code_arr)):
        Need_code_arr[i] = identify_code_in_memory(Need_code_arr[i]) 
    if len(Motivation_item['Action'].iloc[0])==0:
        Action_code = need_to_action(Need_code_arr[0])
        if Action_code is None: # default approach if no any actions found
            Memory_Node = MemoryNode()
            Memory_Node.Name = "Default Action"
            Key_Code = np.random.randint(low=0, high=100, size=SEGMENT)/100 # placeholder, needs to create convertion function
            Memory_Node.Key_Code = Key_Code
            Memory_Node.Type = ActionType.Default
            Action_code = Memory_Node
    else:
        Action_code_arr = env_organs_to_memory_code(Motivation_item['Action'].iloc[0], NodeType.Action) 
        for i in range(0, len(Action_code_arr)):
            Action_code_arr[i] = identify_code_in_memory(Action_code_arr[i])
            Nodes_arr = set_link_for_nodes(Need_code_arr[0], Action_code_arr[i], LinkType.Next)
    return Need_code_arr, Action_code_arr


def Do_Check_func(Action_code_arr, Motivation_item):
    Reaction_code_arr = env_reaction_on_action(Action_code_arr, Motivation_item['Time'].iloc[0])
    Reaction_Object_code = sensory_perception_to_object(Reaction_code_arr)
    Result = Result_Global[Result_Global['Name']==Motivation_item['Result'].iloc[0]]["Result"].iloc[0]
    Name_Type_Results = Result_Global[Result_Global['Name']==Motivation_item['Result'].iloc[0]]
    Types = Name_Type_Results['Result_Type'].iloc[0]
    Results = Name_Type_Results['Result'].iloc[0]
    Reaction_Result_arr = []
    for i in range(0, len(Types)):
        Result = Results[i][0]
        Memory_Node_temp = MemoryNode()
        Memory_Node_temp.Name = Result
        if Types[i] == ResultType.Visual.value:
            Memory_Node_temp.Type = ResultType.Visual
            if Result == 'Object is identified': # example how can interpret
                for i in range(0, len(Reaction_Object_code.Link_IDs)):
                    Link_Type_Check = (Memory_Link_Global[Reaction_Object_code.Link_IDs[i]].Type == LinkType.ObjectFor)
                    Reaction_Type_Check = (Memory_Node_Global[Memory_Link_Global[Reaction_Object_code.Link_IDs[i]].Dist_Node_ID].Type == ReactionType.Visual)
                    if Link_Type_Check and Reaction_Type_Check:
                        Memory_Node_temp.Match_Ratio = 1
        elif Types[i] == ResultType.Hearing.value:
            Memory_Node_temp.Type = ResultType.Hearing
            Memory_Node_temp.Match_Ratio = 1 # placeholder achieving result
        elif Types[i] == ResultType.Touch.value:
            Memory_Node_temp.Type = ResultType.Touch
            if Motivation_item['Time'].iloc[0] ==3: # placeholder achieving result
                Memory_Node_temp.Match_Ratio = 1 
        Reaction_Result_arr.append(Memory_Node_temp)
    return Reaction_Result_arr

def Check_Act_func(Motivation_item, Reaction_Result_arr, Action_code_arr):
    global Motivation_Global
    global Environment_Global
    Current_Time = Motivation_item['Time'].iloc[0]
    Next_Time = Current_Time + 1
    Motivation_item_copy = Motivation_item.copy()
    Motivation_item_copy['Time'] = Next_Time
    Motivation_Global = pd.concat([Motivation_Global, Motivation_item_copy], ignore_index=True)
    Environment_Global_next = Environment_Global.loc[Environment_Global['Time']==Next_Time]
    if Environment_Global_next.size == 0:
        Environment_Global_copy = Environment_Global.loc[Environment_Global['Time']==Current_Time].copy()
        Environment_Global_copy['Time'] = Next_Time
        Environment_Global = pd.concat([Environment_Global, Environment_Global_copy], ignore_index=True)
    Is_Result_Achieved_arr = []
    for i in range(0, len(Reaction_Result_arr)):
        if Reaction_Result_arr[i].Type == ResultType.Visual:
            if Motivation_Global.loc[Motivation_Global['Time']==Next_Time,'Status'].iloc[0][0][0] == ResultType.Visual.value:
                if Reaction_Result_arr[i].Match_Ratio >= MATCH_RATIO:
                    Motivation_Global.loc[Motivation_Global['Time']==Next_Time,'Status'].iloc[0][0][1] = 'Achieved'
                    Is_Result_Achieved_arr.append(True)
                    # boost links in case of success
                    set_link_for_nodes(Action_code_arr[i], Reaction_Result_arr[i], LinkType.Next)
                    set_link_for_nodes(Reaction_Result_arr[i], Action_code_arr[i], LinkType.Previous)
                else:
                    Is_Result_Achieved_arr.append(False)
                    Motivation_Global.loc[Motivation_Global['Time']==Next_Time,'Status'].iloc[0][0][1] = 'InProcess'
                    # ToDo - correct actions to achive result
        elif Reaction_Result_arr[i].Type == ResultType.Hearing:
            if Motivation_Global.loc[Motivation_Global['Time']==Next_Time,'Status'].iloc[0][1][0] == ResultType.Hearing.value:
                if Reaction_Result_arr[i].Match_Ratio >= MATCH_RATIO:
                    Motivation_Global.loc[Motivation_Global['Time']==Next_Time,'Status'].iloc[0][1][1] = 'Achieved'
                    Is_Result_Achieved_arr.append(True)
                    # boost links in case of success
                    set_link_for_nodes(Action_code_arr[i], Reaction_Result_arr[i], LinkType.Next)
                    set_link_for_nodes(Reaction_Result_arr[i], Action_code_arr[i], LinkType.Previous)
                else:
                    Is_Result_Achieved_arr.append(False)
                    Motivation_Global.loc[Motivation_Global['Time']==Next_Time,'Status'].iloc[0][1][1] = 'InProcess'
                    # ToDo - correct actions to achive result
        elif Reaction_Result_arr[i].Type == ResultType.Touch:
            if Motivation_Global.loc[Motivation_Global['Time']==Next_Time,'Status'].iloc[0][2][0] == ResultType.Touch.value:
                if Reaction_Result_arr[i].Match_Ratio >= MATCH_RATIO:
                    Motivation_Global.loc[Motivation_Global['Time']==Next_Time,'Status'].iloc[0][2][1] = 'Achieved'
                    Is_Result_Achieved_arr.append(True)
                    # boost links in case of success
                    set_link_for_nodes(Action_code_arr[i], Reaction_Result_arr[i], LinkType.Next)
                    set_link_for_nodes(Reaction_Result_arr[i], Action_code_arr[i], LinkType.Previous)
                else:
                    Is_Result_Achieved_arr.append(False)
                    Motivation_Global.loc[Motivation_Global['Time']==Next_Time,'Status'].iloc[0][2][1] = 'InProcess'
                    # ToDo - correct actions to achive result
    return Is_Result_Achieved_arr


def env_reaction_on_action(Action_code_arr, v_t):
    Reaction_code_arr = []
    for i in range(0, len(Action_code_arr)):
        Action_Name = Action_code_arr[i].Name
        Reaction_Name = Environment_Global[(Environment_Global['Action'].str.contains(str(Action_Name))) & (Environment_Global['Time']==v_t)]["Reaction"].iloc[0]
        Reaction_Node = MemoryNode()
        Reaction_Node.Name = Reaction_Name
        Reaction_Node_code_arr = env_organs_to_memory_code(Reaction_Name, NodeType.Reaction)
        Reaction_Node = Reaction_Node_code_arr[0]
        Reaction_Node = identify_code_in_memory(Reaction_Node)
        set_link_for_nodes(Action_code_arr[i], Reaction_Node, LinkType.Next)
        Reaction_code_arr.append(Reaction_Node)
    return Reaction_code_arr


def env_organs_to_memory_code(Input_Name, Node_Type = NodeType.Other):
    Nodes_arr = []
    if Node_Type == NodeType.Reaction:
        Reaction = Environment_Global_reaction[Environment_Global_reaction['Reaction']==Input_Name]
        Digital_Twin_Index = Reaction["Digital_Twin_Index"].iloc[0]
        Reaction_Type = Reaction["Reaction_Type"].iloc[0]
        Memory_Node = MemoryNode()
        Key_Code = np.arange(SEGMENT, dtype=int) # placeholder, correct way is to prepare custom dataset and convertion from input to Key Code
        Key_Code = np.full_like(Key_Code, Digital_Twin_Index)
        # img, label = training_data[Digital_Twin_Index] 
        # Key_Code = torch.flatten(img.squeeze())[0:SEGMENT]
        if ReactionType.Visual.name == Reaction_Type:
            Memory_Node.Type = ReactionType.Visual
        elif ReactionType.Hearing.name == Reaction_Type:
            Memory_Node.Type = ReactionType.Hearing
        elif ReactionType.Touch.name == Reaction_Type:
            Memory_Node.Type = ReactionType.Touch
        else:
            Memory_Node.Type = Node_Type
        Memory_Node.Key_Code = Key_Code
        Memory_Node.Name = Input_Name
        Nodes_arr.append(Memory_Node)
    elif Node_Type == NodeType.Action:
        Name_Type_Actions = Action_Global[Action_Global['Name']==Input_Name]
        Types = Name_Type_Actions['Action_Type'].iloc[0]
        Actions = Name_Type_Actions['Action'].iloc[0]
        Digital_Twin_Indexs = Name_Type_Actions['Digital_Twin_Index'].iloc[0]
        for i in range(0, len(Types)):
            # take only the first action, correct way is to take all and check 
            Action = Actions[i][0]
            Memory_Node = MemoryNode()
            Memory_Node.Name = Action
            Digital_Twin_Index = Digital_Twin_Indexs[i][0]
            Key_Code = np.arange(SEGMENT, dtype=int) # placeholder, correct way is to prepare custom dataset and convertion from input to Key_Code
            Key_Code = np.full_like(Key_Code, Digital_Twin_Index)
            if Types[i] == ActionType.Eye.value:
                Memory_Node.Type = ActionType.Eye
            elif Types[i] == ActionType.Ear.value:
                Memory_Node.Type = ActionType.Ear
            elif Types[i] == ActionType.Hand.value:
                Memory_Node.Type = ActionType.Hand
            elif Types[i] == ActionType.Brain.value:
                Memory_Node.Type = ActionType.Brain
            Memory_Node.Key_Code = Key_Code
            Nodes_arr.append(Memory_Node)
    else:
        Memory_Node = MemoryNode()
        Key_Code = np.random.randint(low=0, high=100, size=SEGMENT)/100
        Memory_Node.Key_Code = Key_Code
        Memory_Node.Name = Input_Name
        Memory_Node.Type = Node_Type
        Nodes_arr.append(Memory_Node)
    return Nodes_arr

def need_to_action(Need_code):
    Linked_Edges = find_linked_edges(Need_code, LinkType.Next)
    if len(Linked_Edges)!=0:
        Maximum_Weight_Index = len(Linked_Edges) - 1
        Action_code = Memory_Node_Global[Linked_Edges[Maximum_Weight_Index].Dist_ID]
    else:
        Action_code = None
    return Action_code


def sensory_perception_to_object(Reaction_code_arr):
    global NODE_ID
    global LINK_ID
    global Memory_Node_Global
    global Memory_Link_Global
    Sensories_code_arr = {}
    Memory_Node_Object_temp = MemoryNode()
    Is_Found = False
    for Reaction in Reaction_code_arr:
        Sensories_code_arr[Reaction.Node_ID] = Reaction
    for Node_ID, Memory_Node in Memory_Node_Global.items():
        if (Memory_Node.Type == NodeType.Object):
            Linked_Edges = find_linked_edges(Memory_Node, LinkType.ObjectFor)
            for i in range(0, len(Linked_Edges)):
                if Sensories_code_arr.get(Linked_Edges[i].Dist_Node_ID) is not None:
                    Is_Found = True     
                    Memory_Node_Object_temp = Memory_Node
                    break
    if not Is_Found:
        NODE_ID = NODE_ID + 1
        Memory_Node_Object_temp.Node_ID = NODE_ID
        Memory_Node_Object_temp.Type = NodeType.Object
        for Node_ID, Memory_Node in Sensories_code_arr.items():
            Memory_Node_Object_temp.Name = Memory_Node_Object_temp.Name +'_'+ Memory_Node.Name
            LINK_ID = LINK_ID + 1
            Memory_Link_temp = LinksEdge()
            Memory_Link_temp.Edge_ID = LINK_ID
            Memory_Link_temp.Source_Node_ID = Memory_Node_Object_temp.Node_ID
            Memory_Link_temp.Dist_Node_ID = Node_ID
            Memory_Link_temp.Type = LinkType.ObjectFor
            Memory_Link_temp.Weights = calc_wieght_increase(Memory_Link_temp.Weights)
            Memory_Link_Global[Memory_Link_temp.Edge_ID] = Memory_Link_temp
            Memory_Node_Object_temp.Link_IDs.append(Memory_Link_temp.Edge_ID)
        Nodes_arr = env_organs_to_memory_code(Memory_Node_Object_temp.Name, NodeType.Object)
        Memory_Node_Object_temp.Key_Code = Nodes_arr[0].Key_Code
        Memory_Node_Global[Memory_Node_Object_temp.Node_ID] = Memory_Node_Object_temp
    return Memory_Node_Object_temp

# run function for test
curiosity_behavior(1, 3)

print("End!")