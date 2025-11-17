# Definition for singly-linked list.
from types import Optional

class ListNode:
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next

class Solution:
    def addTwoNumbers(self, l1: Optional[ListNode], l2: Optional[ListNode]) -> Optional[ListNode]:
        remainder = 0
        
        # first two numbers
        val = (l1.val + l2.val) % 10
        remainder = (l1.val + l2.val) // 10

        l1 = l1.next
        l2 = l2.next
        
        if not l1 and not l2 and not remainder:
            return ListNode(val, None)
    
        current_node = ListNode()
        output_node = ListNode(val, current_node)
        while True:
            val = remainder

            if l1:
                val += l1.val
                
            if l2:
                val += l2.val
            
            current_node.val = val % 10 
            
            if val >= 10:
                remainder = 1
            else:
                remainder = 0
                
            if l1:
                l1 = l1.next
            
            if l2:
                l2 = l2.next
            
            if l1 or l2 or remainder:
                new_node = ListNode()
                current_node.next = new_node
                current_node = new_node
            else:
                break
            
        return output_node
