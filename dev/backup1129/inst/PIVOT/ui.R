# Copyright (c) 2015,2016, Qin Zhu and Junhyong Kim, University of Pennsylvania.
# All Rights Reserved.
#
# You may not use this file except in compliance with the Kim Lab License
# located at
#
#     http://kim.bio.upenn.edu/software/LICENSE
#
# Unless required by applicable law or agreed to in writing, this
# software is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied.  See the License
# for the specific language governing permissions and limitations
# under the License.


for(file in list.files("src/helper_function",
                       pattern="\\.(r|R)$",
                       full.names = TRUE)) {
    
    source(file, local = TRUE)
}

for(file in list.files("src/ui_components",
                       pattern="\\.(r|R)$",
                       full.names = TRUE)) {
    
    source(file, local = TRUE)
}


############################### Combine all ##################################
dashboardPage(
    skin = "purple",
    dbHeader,
    sidebar,
    body
)

