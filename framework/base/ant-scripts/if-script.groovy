/*
This file is subject to the terms and conditions defined in
file 'LICENSE', which is part of this source code package.
*/

def condition = elements.condition
def commands
if (condition[0].eval()) {
 commands = elements.commands
} else {
 commands = elements.else
}
if (commands && !commands.isEmpty()) commands[0].execute()
