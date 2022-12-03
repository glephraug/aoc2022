package main

import (
   "fmt"
   "bufio"
   "os"
)

func charval(c rune) int {
   i := int(c)
   if i >= int('a') && i <= int('z') {
      return i - int('a') + 1
   } else if i >= int('A') && i <= int('Z') {
      return i - int('A') + 27
   } else {
      return 0
   }
}


func stringset(s string) map[int]bool {
   set := make(map[int]bool)
   for _,r := range s {
      set[charval(r)] = true
   }
   return set
}


func union(a map[int]bool, b map[int]bool) map[int]bool {
   c := make(map[int]bool)
   for i,_ := range a {
      exists,_ := b[i]
      if exists {
         c[i] = true
      } 
   }
   return c
}


func part1() {
   file, _ := os.Open("input3.txt")
   scanner := bufio.NewScanner(file)
   total := 0
   for scanner.Scan() {
      s := scanner.Text()
      l := len(s)
      shared := union(stringset(s[:l/2]), stringset(s[l/2:]))
      for i,_ := range shared {
         total = total + i
      }
   }
   fmt.Println(total)
}


func part2() {
   file, _ := os.Open("input3.txt")
   scanner := bufio.NewScanner(file)
   var lines []string
   total := 0
   for scanner.Scan() {
      s := scanner.Text()
      lines = append(lines, s)
   }
   for i := 0; i < len(lines); i+=3 {
      shared := union(union(stringset(lines[i]), stringset(lines[i+1])), stringset(lines[i+2]))
      for i,_ := range shared {
         total = total + i
      }
   }
   fmt.Println(total)
}


func main() {
   part1()
   part2()
}


