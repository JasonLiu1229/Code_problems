class Solution:
    # def isPalindrome(self, x: int) -> bool:
    #     string_x = str(x)
        
    #     begin = 0
    #     end = len(string_x) - 1
        
    #     while begin < end:
    #         if string_x[begin] != string_x[end]:
    #             return False
    #         begin += 1
    #         end -= 1
    #     return True
    
    def isPalindrome(self, x: int) -> bool:
        if x < 0:
            return False
        
        if x % 10 == 0 and x != 0:
            return False
        
        reversed_half = 0
        while x > reversed_half:
            # multiply by 10
            reversed_half *= 10
            
            # add remainder  
            reversed_half += x % 10
            
            # remove remainder to and make it 10 times smaller
            x //= 10

        if x == reversed_half: # for even numbers of digits
            return True
        if x == reversed_half // 10: # For odd numbers of digits
            return True
        return False
