# Combinators

Determine if each of the following are combinators or not.

1. **𝜆𝑥.𝑥𝑥𝑥**

   **Yes**. The body only contains *𝑥*, which is also in the head.

2. **𝜆𝑥𝑦.𝑧𝑥**

   **No**. *𝑧* is a free variable.

3. **𝜆𝑥𝑦𝑧.𝑥𝑦(𝑧𝑥)**

   **Yes**. All three variables contained in the body are also in the head.

4. **𝜆𝑥𝑦𝑧.𝑥𝑦(𝑧𝑥𝑦)**

   **Yes**. All three variables contained in the body are also in the head.

5. **𝜆𝑥𝑦.𝑥𝑦(𝑧𝑥𝑦)**

   **No**. *𝑧* is a free variable.

# Normal form or diverge?

Determine if each of the following can be reduced to a normal form or if they diverge.

1. **𝜆𝑥.𝑥𝑥𝑥**

   Is already in **normal form**.

2. **(𝜆𝑧.𝑧𝑧)(𝜆𝑦.𝑦𝑦)**

   **Diverges**, as it is the *omega* term (alpha-equivalent to *(𝜆𝑥.𝑥𝑥)(𝜆𝑥.𝑥𝑥)*).

3. **(𝜆𝑥.𝑥𝑥𝑥)𝑧**

   **Can be reduced** to *𝑧𝑧𝑧*.

# Beta reduce

Evaluate (that is, beta reduce) each of the following expressions to normal form. We strongly recommend writing out the steps on paper with a pencil or pen.

1. **(𝜆𝑎𝑏𝑐.𝑐𝑏𝑎)𝑧𝑧(𝜆𝑤𝑣.𝑤)**
   
   (𝜆𝑎𝑏𝑐.𝑐𝑏𝑎)𝑧𝑧(𝜆𝑤𝑣.𝑤)  
   (𝜆𝑏𝑐.𝑐𝑏𝑧)𝑧(𝜆𝑤𝑣.𝑤)  
   (𝜆𝑐.𝑐𝑧𝑧)(𝜆𝑤𝑣.𝑤)  
   (𝜆𝑤𝑣.𝑤)𝑧𝑧  
   (𝜆𝑣.𝑧)𝑧  
   **𝑧**

2. **(𝜆𝑥.𝜆𝑦.𝑥𝑦𝑦)(𝜆𝑎.𝑎)𝑏**
   
   (𝜆𝑥.𝜆𝑦.𝑥𝑦𝑦)(𝜆𝑎.𝑎)𝑏  
   (𝜆𝑦.(𝜆𝑎.𝑎)𝑦𝑦)𝑏  
   (𝜆𝑎.𝑎)𝑏𝑏  
   **𝑏𝑏**
   
3. **(𝜆𝑦.𝑦)(𝜆𝑥.𝑥𝑥)(𝜆𝑧.𝑧𝑞)**
   
   (𝜆𝑦.𝑦)(𝜆𝑥.𝑥𝑥)(𝜆𝑧.𝑧𝑞)  
   (𝜆𝑥.𝑥𝑥)(𝜆𝑧.𝑧𝑞)  
   (𝜆𝑧.𝑧𝑞)(𝜆𝑧.𝑧𝑞)  
   (𝜆𝑧.𝑧𝑞)𝑞  
   **𝑞𝑞**
   
4. **(𝜆𝑧.𝑧)(𝜆𝑧.𝑧𝑧)(𝜆𝑧.𝑧𝑦)**

   (𝜆𝑧.𝑧)(𝜆𝑧.𝑧𝑧)(𝜆𝑧.𝑧𝑦)  
   (𝜆𝑧.𝑧𝑧)(𝜆𝑧.𝑧𝑦)  
   (𝜆𝑧.𝑧𝑦)(𝜆𝑧.𝑧𝑦)  
   (𝜆𝑧.𝑧𝑦)𝑦  
   **𝑦𝑦**

5. **(𝜆𝑥.𝜆𝑦.𝑥𝑦𝑦)(𝜆𝑦.𝑦)𝑦**

   (𝜆𝑦.(𝜆𝑦.𝑦)𝑦𝑦)𝑦  
   (𝜆𝑦.𝑦)𝑦𝑦  
   **𝑦𝑦**

6. **(𝜆𝑎.𝑎𝑎)(𝜆𝑏.𝑏𝑎)𝑐**

   (𝜆𝑎.𝑎𝑎)(𝜆𝑏.𝑏𝑎)𝑐  
   (𝜆𝑏.𝑏𝑎)(𝜆𝑏.𝑏𝑎)𝑐  
   (𝜆𝑏.𝑏𝑎)𝑎𝑐  
   **𝑎𝑎𝑐**

7. **(𝜆𝑥𝑦𝑧.𝑥𝑧(𝑦𝑧))(𝜆𝑥.𝑧)(𝜆𝑥.𝑎)**

   (𝜆𝑥𝑦𝑧.𝑥𝑧(𝑦𝑧))(𝜆𝑥.𝑧)(𝜆𝑥.𝑎)  
   (𝜆𝑦𝑧'.(𝜆𝑥.𝑧)𝑧'(𝑦𝑧))(𝜆𝑥.𝑎)  
   𝜆𝑧'.(𝜆𝑥.𝑧)𝑧'((𝜆𝑥.𝑎)𝑧)  
   𝜆𝑧'.𝑧((𝜆𝑥.𝑎)𝑧)  
   **𝜆𝑧'.𝑧𝑎**  
   
   Note: We rename *𝑧* into *𝑧'* to avoid name clashes with the free variable *𝑧* from *(𝜆𝑥.𝑧)* after the first beta-reduction.